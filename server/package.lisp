;;;; package.lisp

(defpackage #:lserver
  (:use #:cl)
  (:export #:run-server
           #:*server*
           #:add-command
           #:remove-command
           #:lserver-homedir-pathname))

(defpackage #:lserver-impl
  (:use #:cl
        #:lserver))
