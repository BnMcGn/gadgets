;;;; gadgets.asd

(asdf:defsystem #:gadgets
  :serial t
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities #:anaphora #:alexandria #:uiop #:kebab
               #:cl-hash-util) 
  :components ((:file "package")
               (:file "early")
               (:file "collecting")
               (:file "anaphorics")
               (:file "gadgets" :depends-on ("anaphorics" "early" 
                                             "collecting"))
               (:file "tree")
               (:file "symtools")))

