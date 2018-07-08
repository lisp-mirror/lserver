;;;; lserver.lisp

(in-package #:lserver-impl)

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
  (if (uiop:file-exists-p *rc-file*)
      (load *rc-file*)
      (warn "Initialization file ~A does not exist." *rc-file*))
  (unless (slot-boundp server 'socket)
    (setf (slot-value server 'socket) (setup-socket (socket-address server))))
  server)

(defun start-server (server)
  (catch 'stop-server
         (loop for sock = (sb-bsd-sockets:socket-accept (socket server))
               do (bt:make-thread (default-connection-handler sock) :name "lserver worker"))))

(defvar *arguments* nil)
(defvar *client-name* "")

(defparameter *handler* (lambda () (standard-handler *arguments*)))

(defun default-connection-handler (socket)
  (lambda ()
    (let ((*interpreter-stream* (sb-bsd-sockets:socket-make-stream socket
                                                                   :input t
                                                                   :output t
                                                                   :element-type '(unsigned-byte 8)
                                                                   :auto-close t)))
      (let ((*standard-input* (flexi-streams:make-flexi-stream (make-server-binary-input-stream *interpreter-stream*)
                                                               :external-format :utf-8))
            (*standard-output*
              (flexi-streams:make-flexi-stream
                (make-server-binary-output-stream *interpreter-stream* 
                                                  :stdout
                                                  (if (isatty)
                                                      :line-buffered
                                                      :fully-buffered))
                :external-format :utf-8))
            (*error-output*
              (flexi-streams:make-flexi-stream
                (make-server-binary-output-stream *interpreter-stream* 
                                                  :stderr
                                                  :unbuffered)
                :external-format :utf-8))
            (*query-io*
              (make-two-way-stream
                (flexi-streams:make-flexi-stream (make-server-binary-input-stream *interpreter-stream*)
                                                 :external-format :utf-8)
                (flexi-streams:make-flexi-stream
                  (make-server-binary-output-stream *interpreter-stream* 
                                                    :stderr
                                                    :line-buffered)
                  :external-format :utf-8)))
            (*default-pathname-defaults* (pathname (getcwd)))
            (*arguments* (argv))
            (*client-name* (argv0))
            (*package* (find-package "LSERVER"))
            code)
        (unwind-protect
          (progn
            ;;; if we can't pass the errors to the client, something must be wrong with the connection, so we don't care
            (ignore-errors
              ;;; the handler-case assumes we are able to tell the client something about the error
              (handler-case
                (let ((result (funcall *handler*)))
                  (setf code (typecase result
                               (integer result)
                               (null 1)
                               (t 0))))
                (simple-error (c) (princ c *error-output*))
                (file-error (c) (format *error-output* "Error with file ~A.~%" (file-error-pathname c)))
                (error () (sb-debug:print-backtrace :stream *error-output*))))
            (ignore-errors (finish-output *standard-output*))
            (ignore-errors (finish-output *error-output*))
            (ignore-errors (finish-output *query-io*))
            (ignore-errors (quit-interpreter (or code 255))))
          (ignore-errors (close *interpreter-stream*))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))))

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


(defvar *server*)

(defun awesome (&key (background t) socket-address (rc-file *rc-file*))
  (setf *server* (let ((*rc-file* rc-file))
                   (setup-server (make-server socket-address))))
  (if background
      (bt:make-thread (lambda () (start-server *server*)) :name "lserver main thread")
      (start-server *server*)))
