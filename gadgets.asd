;;;; gadgets.asd

(asdf:defsystem #:gadgets
  :serial t
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities #:anaphora #:alexandria #:uiop #:kebab)
  :components ((:file "package")
               (:file "early")
               (:file "anaphorics")
               (:file "gadgets" :depends-on ("anaphorics" "early"))
               (:file "tree")))

