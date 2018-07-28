;;;; lserver.lisp

(in-package #:lserver-impl)

(defparameter *lserver-home* (merge-pathnames #p".lserver/" (user-homedir-pathname)))
(defparameter *default-socket* #p"default")
(defparameter *directories* '(#p"tmp/"
                              #p"commands/"))

(defclass lserver ()
  ((home :reader server-home :initarg :home :initform *lserver-home*)
   (socket :reader socket)
   (socket-file :reader socket-file :initarg :socket-file :initform *default-socket*)))

(defgeneric rc-file (server))

(defmethod rc-file ((server lserver))
  (merge-pathnames #p"lserverrc.lisp" (server-home server)))

(defgeneric socket-address (server))

(defmethod socket-address ((server lserver))
  (namestring (merge-pathnames  (socket-file server) (merge-pathnames #p"tmp/" (server-home server)))))

(defun make-server (&key (home (or (uiop:getenv "LSERVER_HOME")
                                   *lserver-home*))
                         (socket-file (or (uiop:getenv "LSERVER_SOCKET")
                                          *default-socket*)))
  (make-instance 'lserver :home home :socket-file socket-file))

(defun server-shutdown (server)
  (let ((socket (socket server)))
    (sb-bsd-sockets:socket-shutdown socket :direction :io)
    (sb-bsd-sockets:socket-close socket)))

(defun setup-directories (home)
  (dolist (dir *directories*)
    (ensure-directories-exist (merge-pathnames dir home))))

;;; TODO reset the socket?
(defun setup-server (server)
  (setup-directories (server-home server))
  (let ((rc-file (rc-file server)))
    (if (uiop:file-exists-p rc-file)
        (load rc-file)
        (warn "Initialization file ~A does not exist." rc-file)))
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

(defun run-server (&key background (socket-file *default-socket*) (home *lserver-home*))
  (setf *server* (setup-server (make-server :home home :socket-file socket-file)))
  (if background
      (bt:make-thread (lambda () (start-server *server*)) :name "lserver main thread")
      (start-server *server*)))
