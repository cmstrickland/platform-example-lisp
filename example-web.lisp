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
