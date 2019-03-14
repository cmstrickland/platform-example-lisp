### Example common lisp web project for deploying on [platform.sh](https://platform.sh)

- It's a hello world built using clack
- bootstrap is via roswell , which installs sbcl and also compiles our system to a binary
- the platform build hook script downloads and installs roswell and then points that at the .ros script
- platform config files are provided to connect up the web app to serve https

Platform.sh does not natively supply a common lisp application container, but the build hook allows you to run scripts ahead of deploy, and this project is provided with one which bootstraps a lisp environment for the code to deploy into by installing [roswell](https://github.com/roswell/roswell)

If you setup a platform.sh project and then push this repo to it, the lisp webapp will be automagically compiled and deployed.

(deploy hook script cribbed from [@ralt](https://github.com/ralt)'s earlier version)
