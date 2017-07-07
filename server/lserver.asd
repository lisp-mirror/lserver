;;;; lserver.asd

(asdf:defsystem #:lserver
  :description "Describe lserver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on ("trivial-gray-streams" "bordeaux-threads" "trivial-utf-8")
  :serial t
  :components ((:file "package")
               (:file "lserver-streams")))

