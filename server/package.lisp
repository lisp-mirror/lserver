;;;; package.lisp

(defpackage #:lserver-communication
  (:use #:cl)
  (:export #:make-stream-with-lock
           #:make-session-input-stream
           #:make-session-output-stream
           #:order
           #:exit
           #:read-character
           #:read-line
           #:print-stdout
           #:print-stderr
           #:cwd
           #:program-name
           #:lisp-args 
           #:query-cwd
           #:query-program-name
           #:query-lisp-args 
           #:*buffer-size*
           #:*header-size*
           #:*small-buffer-size*
           #:communication-error 
           #:unknown-message-type-code
           #:broken-pipe
           #:corrupt-data  
           #:corrupt-header
           #:type-mismatch
           #:client-error
           #:flush-error
           #:write-error))
