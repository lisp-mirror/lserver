;;;; lserver.lisp

(defpackage #:lserver (:use #:cl))

(in-package #:lserver)

(defclass lserver ()
  ((socket :reader socket)
   (socket-address :reader socket-address :initarg :socket-address :initform (infer-socket-address))))

(defun make-server (&optional socket-address)
  (let* ((socket-address (infer-socket-address socket-address))
         (server (make-instance 'lserver :socket-address socket-address)))
    server))

(defun server-shutdown (server)
  (let ((socket (socket server)))
    (sb-bsd-sockets:socket-shutdown socket :direction :io)
    (sb-bsd-sockets:socket-close socket)))

(defparameter *rc-file* (merge-pathnames ".lserverrc" (user-homedir-pathname)))

;;; TODO reset the socket?
(defun setup-server (server)
  (load *rc-file* :if-does-not-exist nil)
  (unless (slot-boundp server 'socket)
    (setf (slot-value server 'socket) (setup-socket (socket-address server))))
  server)

;;; TODO thread pool
(defun start-server (server)
  (catch 'stop-server
         (loop for sock = (sb-bsd-sockets:socket-accept (socket server))
               do (bt:make-thread (default-connection-handler sock) :name "lserver worker"))))

(defpackage "LSERVER-CALLER" (:use #:cl))

(defvar *arguments* nil)
(defvar *client-name* "")

(defun default-connection-handler (socket)
  (lambda ()
    (let* ((socket-stream (lserver-communication:make-stream-with-lock (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type '(unsigned-byte 8))))
           (*standard-input* (lserver-communication:make-session-input-stream socket-stream))
           (*standard-output* (lserver-communication:make-session-output-stream socket-stream 'lserver-communication:print-stdout))
           (*error-output* (lserver-communication:make-session-output-stream socket-stream 'lserver-communication:print-stderr))
           (*query-io* (make-two-way-stream (lserver-communication:make-session-input-stream socket-stream) (lserver-communication:make-session-output-stream socket-stream 'lserver-communication:print-stderr)))
           code)
      (unwind-protect
        (prog1
          (handler-case
            (let ((*default-pathname-defaults* (pathname (lserver-communication:query-cwd *standard-input*)))
                  (*arguments* (lserver-communication:query-lisp-args *standard-input*))
                  (*client-name* (lserver-communication:query-program-name *standard-input*))
                  (*package* (find-package "LSERVER-CALLER")))
              (let ((result (funcall *handler*)))
                (setf code (typecase result
                             (integer result)
                             (null 1)
                             (t 0)))))
            (lserver-communication:communication-error () (format *terminal-io* "Communication error.~%"))
            (lserver-communication:client-error () (format *terminal-io* "Client error.~%"))
            (simple-error (c) (princ c *error-output*))
            (file-error (c) (format *error-output* "Error with file ~A.~%" (file-error-pathname c)))
           ; (error () (write-line "ERROR!" *error-output*))
            )
          (ignore-errors (finish-output *standard-output*))
          (ignore-errors (finish-output *error-output*))
          (ignore-errors (lserver-communication:order *standard-input* 'lserver-communication:exit (or code -1))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

;;; TODO permissions
;;; TODO treat socket directories & socket names separately
(defun setup-socket (file)
  (ensure-directories-exist (pathname file))
  (uiop:delete-file-if-exists file)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind socket file)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defparameter *default-socket* (namestring (merge-pathnames ".lserver/default" (user-homedir-pathname))))

(defun infer-socket-address (&optional name)
  (or name
      (uiop:getenv "LSERVER_SOCKET")
      *default-socket*))

(defstruct command name function description)

(defstruct command-set
  (name "command set")
  (lock (bt:make-lock "command lock"))
  (commands (make-hash-table :test 'equal)))

(defvar *commands* (make-command-set))

(defun add-command (name function &optional description (commands *commands*))
  (check-type name string)
  (check-type function (or symbol function))
  (check-type description (or string null))
  (unless description
    (setf description (documentation function 'function)))
  (let ((command (make-command :name name
                               :function function
                               :description description)))
    (bt:with-lock-held ((command-set-lock commands))
      (setf (gethash name (command-set-commands commands)) command))))

(defun remove-command (name &optional (commands *commands*))
  (bt:with-lock-held ((command-set-lock commands))
    (remhash name (command-set-commands commands))))

(defun get-command (name &optional (commands *commands*))
  (bt:with-lock-held ((command-set-lock commands))
    (values (gethash name (command-set-commands commands)))))

(defun standard-handler (arguments)
  (if arguments
      (let* ((command-name (first arguments))
             (args (rest arguments))
             (command (get-command command-name)))
        (if command
            (funcall (command-function command) args)
            (error "Unknown command: ~A.~%" command-name)))
      (error "Command missing.~%")))

(defparameter *handler* (lambda () (standard-handler *arguments*)))

(defvar *server*)

(defun awesome (&optional background)
  (setf *server* (setup-server (make-server)))
  (if background
      (bt:make-thread (lambda () (start-server *server*)) :name "lserver main thread")
      (start-server *server*)))
