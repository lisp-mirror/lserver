(in-package #:cl-user)

#-sbcl
(error "This lisp implementation is not supported.")

(ql:quickload "lserver")

(sb-ext:save-lisp-and-die "lserver" :toplevel (lambda ()
                                                (handler-case
                                                  (lserver:run-server)
                                                  (sb-sys:interactive-interrupt () 0))
                                                0)
                          :executable t
                          :purify t)
