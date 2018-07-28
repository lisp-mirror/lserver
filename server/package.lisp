;;;; package.lisp

(defpackage #:lserver
  (:use #:cl)
  (:export #:run-server
           #:*server*
           #:add-command
           #:remove-command))

(defpackage #:lserver-impl
  (:use #:cl
        #:lserver))
