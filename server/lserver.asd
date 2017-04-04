;;;; lserver.asd

(asdf:defsystem #:lserver
  :description "Describe lserver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on ("trivial-gray-streams")
  :serial t
  :components ((:file "package")
               (:file "lserver")))

