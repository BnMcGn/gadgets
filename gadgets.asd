;;;; gadgets.asd

(asdf:defsystem #:gadgets
  :serial t
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities #:anaphora #:alexandria)
  :components ((:file "package")
               (:file "gadgets")
               (:file "tree")
               (:file "anaphorics")))

