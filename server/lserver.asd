;;;; lserver.asd

(asdf:defsystem #:lserver
  :description "A Lisp server executing software on demand"
  :author "Stanislav Kondratyev <kondratjevsk@gmail.com>"
  :license "CC0"
  :depends-on ("trivial-gray-streams" "bordeaux-threads" "trivial-utf-8")
  :serial t
  :components ((:file "package")
               (:file "communication")
               (:file "lserver")))

