(defpackage "platform-example-lisp"
  (:use #:cl))

(defun hello-world (env)
  (declare (ignore env))
  '(200 (:content-type "text/html") ("<h1>It's alive!</h1>Hello, world!")))

(defun env-lookup-port ()
  (let ((env-var (ccl:getenv "PORT")))
    (cond (env-var (parse-integer env-var))
          (t 5000))))

(defvar *handler*
  (clack:clackup #'hello-world :port (env-lookup-port)))
