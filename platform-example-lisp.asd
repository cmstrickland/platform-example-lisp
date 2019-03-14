(defsystem "platform-example-lisp"
  :description "example web application"
  :author "cms <cms@beatworm.co.uk>"
  :depends-on ( "hunchentoot" "clack")
  :components ((:file "example-web")))
