;;;; package.lisp

(defpackage #:lserver
  (:use #:cl)
  (:export #:add-command
           #:remove-command))

(defpackage #:lserver-impl
  (:use #:cl
        #:lserver))
