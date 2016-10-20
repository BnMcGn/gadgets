;;;; test-gadgets.asd

(asdf:defsystem #:test-gadgets
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:prove) 
  :serial t
  :components ((:module :t
                        :serial t
                        :components ((:file "test-gadgets")))))


