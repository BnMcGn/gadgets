;;;; gadgets.asd

(asdf:defsystem #:gadgets
  :serial t
  :description "Ben McGunigle's utility collection"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:alexandria #:uiop #:cl-hash-util) 
  :components ((:file "package")
               (:file "early")
               (:file "gadgets" :depends-on ("early"))))

