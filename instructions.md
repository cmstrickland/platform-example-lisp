## Goal

How to run a simple web applications, using the programmable programming language, common lisp, on  platform.sh!

## Assumptions

To complete this task you will need

- a basic understanding of common lisp
- a platform.sh account and the platform CLI tool, and working SSH credentials
- [sbcl](http://www.sbcl.org/) and an editing environment. I used sbcl on debian linux with emacs.
- [quicklisp](http://beta.quicklisp.org) , to manage our packages
- [roswell](https://github.com/roswell/roswell), to launch and package our app

I'm not going to cover installing these basic tools, the documentation links above should explain how. Some or all of them may even be included in your system package manager.

This is a UNIX-centric demonstration, and assumes you run from the `bash` shell. Before we start you should verify that you can do these things.

- run the platform CLI  ( try `platform project:list`)
- run SBCL as a repl ( try `sbcl` )
- run quicklisp (try `sbcl --load "$HOME/quicklisp/setup.lisp"`)
- run roswell ( try `ros help` )

if none of these error we should be good to go. This application should work fine for you on Windows or MacOS, but these instructions have only been tested at a linux shell.

## Problems

- platform.sh doesn't (yet) offer a dedicated common lisp runtime. However, lisp is small and self-hosting, so we can 'borrow' one of the other types and install a lisp compiler at build time.

## Steps

### 1. create a project directory for your app.


First, define a lisp system using `asdf` to make it super-easy to install. If we do this in the quicklisp `local-projects` directory, it's also super-easy to load locally. I have called mine `platform-example-lisp`

Create the file `~/quicklisp/local-projects/platform-example-lisp/platform-example-lisp.asd` containing

```
(defsystem "platform-example-lisp"
  :description "example web application"
  :author "cms <cms@beatworm.co.uk>"
  :depends-on ( "hunchentoot" "clack")
:components ((:file "example-web")))
```

This defines our project, some handy metadata, and it's dependencies in a way that can be easily used by system builders, like quicklisp, or indeed roswell which is what we're going to use to make it extra-simple. Note here we only depend on `clack` the lisp web framework, and specify `hunchentoot` as a the webserver for it to use. That's it!.

The `:components` keyword is a list of our project files. We only have one file in this tiny project, and we haven't written it yet, that will be our step 2.

### 2. write the application code

Now create the `~/quicklisp/local-projects/platform-example-lisp/example-web.lisp` component

```
(defpackage :platform-example-lisp
  (:use #:cl))

(in-package :platform-example-lisp)


(defun hello-world (env)
  "return a friendly HTTP response with a document"
  (declare (ignore env))
  '(200 (:content-type "text/html") ("<h1>It's alive!</h1>Hello, world!")))

(defun env-lookup-port ()
  "get the environment variable PORT as an integer. default to 5000
when unset"
  (let ((env-var (uiop:getenv "PORT")))
    (cond (env-var (parse-integer env-var))
          (t 5000))))

(defun web ()
  "server a web handler forever returning our hello response on root"
  ;; run the web page
  (clack:clackup #'hello-world :port (env-lookup-port))
  ;; run the web server thread as the main thread
  (sb-thread:join-thread
   (find-if
    (lambda (th)
      (search  "clack-handler-hunchentoot" (sb-thread:thread-name th)))
(sb-thread:list-all-threads))))
```

It's a tiny application, because this website is just a very trivial example. It does go to demonstrate how simple lisp web code can be, if you're using some nice lispy abstractions

Let's work through this simple app, which only has three functions

- we declare a package for our app to namespace it, and switch to using it for our definitions
- we define `hello-world` , our spin on the classic "hello world" application, that returns hello world as a HTTP response with a web document.
- we define a convenience function `env-lookup-port` , which we can use to retrieve the PORT variable from the host environment. This will be set for us by the platform runtime. If the environment variable is not set to an number, we default to 5000, for convenience when running
- finally we define `web` which is our application entry point. This just launches clack with our `hello-world` function as the main response handler. This means that http requests to '/' on this server will be served by our handler function.


### 3. Test it out

Run a repl

```
>$ sbcl
This is SBCL 1.4.16.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
```
enter the following form at the `*` prompt
```
* (ql:quickload "platform-example-lisp")

```

you should see quicklisp back-resolving all the dependencies , downloading them and installing them. It turns out that we are actually using far more libraries than just the two we declare, but this is all abstracted away for us by the helpful powers of `asdf` and `quicklisp`

after a short while, the scrolling will stop and you should see something like this
```
To load "platform-example-lisp":
  Load 1 ASDF system:
    platform-example-lisp
; Loading "platform-example-lisp"
......
("platform-example-lisp")
*
```

This indicates that our package is loaded. We can test run our website by calling our `web` function directly, like this

```
* (platform-example-lisp::web)
Hunchentoot server is started.
Listening on localhost:5000.
```

at which point the repl will be taken-over by the server process. If you point a browser at "http://localhost:5000", as instructed, you should see our friendly message in a web page, alongside some self-congratulations (or perhaps I should say warnings)

we don't need the terminal running it any more, you can cancel the app with `Ctrl-C` which will launch a debugger that you can just exit by typing `(quit)` at the prompt.

### 4. Bundle the app using roswell.

As I mentioned before, we won't have a lisp ready for us on the platform host. Neither will we have quicklisp. Because we require these two packages to boot, we will have to install them at build time. The way to do this ahead of deploy is by using a build hook, and we'll dig into that later. First, we're going to make the app even _simpler_ to deploy.

To isolate us from having to manually set up the lisp environment for quicklisp, install our code into the right place, we can leverage another piece of secret alien technology, the roswell utility. This will bundle up our lisp, a quicklisp distribution to manage our dependencies, and also an app launcher, which we can even use to compile the lisp code into a standalone binary application. Pretty cool stuff.

From this point on we will be working in our app directory, so first chdir to it.

```
>$ cd ~/quicklisp/local-projects/platform-example-lisp/

```

Now generate a skeleton roswell script using the `ros` tool

```
>$ ros init example-web
Successfully generated example-web.ros

```

Edit this script to make it look like this. You can leave the preamble alone, we just need to add our package load instructions in place of the dummy `quicklisp` operation, and invoke our application entry point inside the provided `main` form, which is the function the script will run when executed. It needs to look like this


```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(ql:quickload "platform-example-lisp" :silent t))
(defun main (&rest argv)
  (declare (ignorable argv))
  (platform-example-lisp::web))
```

This script makes our application even easier to launch. You just need to do this

```
>$ ros -S "$PWD" example-web.ros
Hunchentoot server is started
Listening on localhost:5000
```

The -S "$PWD" is telling ros to set up asdf to also look in the current working directory. This enables our full boostrap to happen in the ros script with just the `(ql:quickload "platform-example-lisp")`. Unlike previously, this script does not require the application source to be hosted inside the quicklisp local-systems tree. This makes it easier to deploy the app at a different filesystem path.

No repl needed. And now you can just exit with 'CTRL-C'

We can actually take this a stage further, and use roswell to compile this script into an executable.

```
>$ ros -S "$PWD" dump executable example-web.ros -o app
>$ file app
app: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, BuildID[sha1]=8ada62dd26f7ef403aa6aead06d8294fe978eea1, with debug_info, not stripped
>$ ./app
Hunchentoot server is started
Listening on localhost:5000
```

Again, there's our web app, but this time we didn't run any lisp interpreter or load anything. `Ctrl-C` will cancel it once again. Let's run it one more time to just show off our other essential feature.

```
>$ PORT=5001 ./app
Hunchentoot server is started
Listening on localhost:5001
```

This time we have specified a different port number, using the PORT environment variable that you will use in the platform.sh hosted runtime. Check http://localhost:5001 and our page should be there.

So we now have a tiny little web app, and a ridiculously simple build and dependency tool. Next we need a ridiculously simple deploy target.

### 5. Add platform.sh configuration and deploy hooks

We are going to need the usual platform service configuration documents. We've managed to reduce all of our dependencies down to the `roswell` utility, but unfortunately this is not a standard installation on any of the stock platform application images. Not to worry, we can borrow a different one, and we can setup a *build hook* to ensure roswell is ready for us at deploy time. It's just a straightforward shell script. Create this in the project directory as `setup-build-env.sh`

```
#!/usr/bin/env bash

set -e

cd "$PLATFORM_CACHE_DIR"

function build {
    sh bootstrap
    ./configure --prefix ~/.global
    make
}

if [ ! -d roswell ]; then
    git clone --branch release https://github.com/roswell/roswell.git
    cd roswell
    build
else
    cd roswell
    old_head=$(git rev-parse HEAD)
    git pull origin release
    new_head=$(git rev-parse HEAD)
    if [ ! "$new_head" = "$old_head" ]; then
    git clean -xdf
    build
    fi
fi

make install
ros setup
```

using a standard container cache dir, we check out roswell from upstream if it isn't there already. If it is we update it to the latest release. Afterwards we build and install it. This will add a bit of build time to our deploys when it's running, but we do get a lot of power once this tool is installed. So we tell the platform deploy chain to use it by adding it to our `.platform.app.yaml`. Our app is pretty simple so our configuration is straightforward.

```
name: example-lisp

type: python:3.6

build:
  flavor: none

hooks:
  build: |
    set -e
    ./setup-build-env.sh
    ros --source-registry "$PWD" dump executable example-web.ros -o app
disk: 256

web:
  commands:
start: ./app
```

I'm using the python:3.6 image file, mostly because that's what I usually use on platform.sh, and I know it works here, but we aren't relying on any python software. The only interesting parts here are the hooks and the web command. If you look at the build hook, we are running our roswell bootstrap, to provide us with the `ros` tool. Then all we have to do is use `ros` to dump our executable `app`, the same as before. The only difference being that this time, the `app` binary will be installed inside our platform app container. And that is why our web command can simply be `./app` to run it. PORT will be set, and if we set up the routes, our little application will be running on the internet as simply as 'git push'

`.platform/routes.yaml` just looks like this, the simplest possible

```
"https://{default}/":
  type: upstream
upstream: example-lisp:http
```

and finally we add an empty `.platform/services.yaml` because we're not yet using any other services.


### 6. Deploy it to platform

There's nothing extraordinary to add to this step, just connect git up to a platform.sh project as usual.

- initialize a git repo for the local project , and commit everything
- create a new platform.sh project in your account via the UI or the CLI tool
- add the project git remote to your repo as `platform`
- `>$ git push platform master`

Do note that this push, especially the first one will take a little while, because we have to bootstrap the roswell and lisp environments, and compile the application after it is accepted.


## Conclusion

Platform.sh is one of the easiest ways to deploy a web application directly from a git tree. And common-lisp and clack are one of the easiest ways to build elegant web applications using one of the oldest and most interesting programming languages around. Of course, this sample app doesn't do anything much of interest, but the skeleton here could be extended considerably, without any need to modify the deployment template.

Platform.sh doesn't yet offer common lisp as a supported runtime, but by leveraging the build hook and environment variable configuration for `PORT` and the powerful abstractions of `roswell`, `clack` , `quicklisp`, and `asdf` we can roll our own quite simply.

There's a lot of code samples in this HOWTO, and if you don't feel like typing or pasting them all in the [code for this working project](https://github.com/cmstrickland/platform-example-lisp) is up on github. Thanks for reading.
