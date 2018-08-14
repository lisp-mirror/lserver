;;;; communication.lisp

(in-package #:lserver-impl)

;;;;;;;;;;;;;;;;;;;

(defparameter *buffer-size* 4096)

(defvar *interpreter-stream*)

(defun int-s (integer)
  (check-type integer (unsigned-byte 32))
  (check-type *interpreter-stream* stream)
  (loop for i from 3 downto 0 do
        (write-byte (ldb (byte 8 (* 8 i)) integer) *interpreter-stream*))
  (finish-output *interpreter-stream*))

(defun char-s (octet)
  (check-type octet (unsigned-byte 8))
  (check-type *interpreter-stream* stream)
  (write-byte octet *interpreter-stream*)
  (finish-output *interpreter-stream*))

(defun octets-s (octets &key (start 0) end)
  (check-type octets (vector (unsigned-byte 8)))
  (check-type *interpreter-stream* stream)
  (check-type start (and fixnum (integer 0)))
  (check-type end (or null (and fixnum (integer 0))))
  (when (null end)
    (setf end (length octets)))
  (unless (<= 0 start end (the fixnum (length octets)))
    (error 'type-error
           :datum (cons start end)
           :expected-type `(cons (integer 0 ,(length octets))
                                 (integer ,start ,(length octets)))))
  (assert (<= (the fixnum (- end start)) (expt 2 32)))
  (int-s (- end start))
  (write-sequence octets *interpreter-stream* :start start :end end)
  (finish-output *interpreter-stream*))

(defun int-r ()
  (check-type *interpreter-stream* stream)
  (loop with n fixnum = 0
        repeat 4
        do (setf n (+ (ash n 8)
                      (read-byte *interpreter-stream*)))
        finally (return n)))

;; Returns the length!
(defun octets-r (vector &key (start 0))
  (check-type vector (vector (unsigned-byte 8)))
  (check-type *interpreter-stream* stream)
  (check-type start (and fixnum (integer 0)))
  (assert (<= start (length vector)))
  (let ((length (int-r)))
    (assert (<= length (- (length vector) start)))
    (when (plusp length)
      (unless (= (read-sequence vector *interpreter-stream*
                                :start start
                                :end (+ start length))
                 length)
        (error "CORRUPT DATA")))
    length)) 

#|
Use cases: sending command line arguments & environment
A fresh string is to be consed every time.
It seems all right to use throw-away buffers.
|#


(defun string-r ()
  (check-type *interpreter-stream* stream)
  (let ((length (int-r)))
    (when (plusp length)
      (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence buffer *interpreter-stream*) length)
          (error "CORRUPT DATA"))
        (flexi-streams:octets-to-string buffer :external-format :utf-8)))))

;;;;;;;;;;;;;;;;;;;

(defmacro command-code (name number)
  `(defun ,name ()
     (char-s ,number)
     (finish-output *interpreter-stream*)))
#|
I doubt much can be gained by not flushing the commands right away, so we do that automatically.
|#

(command-code quit-c 0)
(command-code ping-c 1)
(command-code int-sc 2)
(command-code data-sc 3)
(command-code int-rc 4)
(command-code data-rc 5)
(command-code read-stdin-c 6)
(command-code dump-to-stdout-c 7)
(command-code flush-stdout-c 8)
(command-code dump-to-stderr-c 9)
(command-code flush-stderr-c 10)
(command-code string-rc 11)
(command-code save-argv0-c 12)
(command-code save-argc-c 13)
(command-code save-arg-c 14)
(command-code save-env-c 15)
(command-code save-cwd-c 16)
(command-code save-isatty-c 17)

(defun ping ()
  (ping-c)
  (zerop (int-r)))

(defun send-int (integer)
  (int-sc)
  (int-s integer))

(defun send-data (vector &key (start 0) end)
  (data-sc)
  (octets-s vector :start start :end end))

(defun get-int ()
  (int-rc)
  (int-r))

;; Returns the length!
(defun get-data (vector &key (start 0))
  (data-rc)
  (octets-r vector :start start))

;; Returns the length!
(defun get-stdin (vector &key (start 0))
  (read-stdin-c)
  (get-data vector :start start))

(defun get-string ()
  (string-rc)
  (string-r))

(defun send-string (string)
  (check-type string string)
  (assert (<= (length string) (1- *buffer-size*)))
  (setf string (concatenate 'string string #.(string (code-char 0))))
  (send-data (flexi-streams:string-to-octets string :external-format :utf-8)))

(defun argv ()
  (let ((argc (progn
                (save-argc-c)
                (get-int))))
    (loop for i below argc
          do (send-int i)
             (save-arg-c)
          collect (get-string))))

(defun argv0 ()
  (save-argv0-c)
  (get-string))

(defun getenv (name)
  (send-string name)
  (save-env-c)
  (let ((defined? (zerop (get-int))))
    (if defined?
        (get-string)
        nil)))

(defun getcwd ()
  (save-cwd-c)
  (uiop:ensure-directory-pathname (get-string)))

(defun isatty ()
  (save-isatty-c)
  (not (zerop (get-int))))

(defun quit-interpreter (&optional (code 0))
  (flush-stdout-c)
  (flush-stderr-c)
  (send-int code)
  (quit-c))

(defmacro with-interpreter-stream ((stream-form) &body body)
  `(let ((*interpreter-stream* ,stream-form))
     ,@body))

;;;;;;;;;;;;;;;;;;;

(defstruct (octet-buffer (:constructor make-octet-buffer (size &aux (buffer (make-array size :element-type '(unsigned-byte 8))))))
  (buffer (make-array 0 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (refillable-p t :type boolean))

(defun refill (octet-buffer)
  (let ((data-length (get-stdin (octet-buffer-buffer octet-buffer))))
    (setf (octet-buffer-start octet-buffer) 0
          (octet-buffer-end octet-buffer) data-length)
    (when (zerop data-length)
      (setf (octet-buffer-refillable-p octet-buffer) nil))))

(defun octet-buffer-read-byte (buffer)
  (check-type buffer octet-buffer)
  (cond ((< (octet-buffer-start buffer) (octet-buffer-end buffer))
         (locally
           (declare (optimize (speed 3) (safety 0)))
           (prog1
             (aref (octet-buffer-buffer buffer) (octet-buffer-start buffer))
             (the fixnum (incf (octet-buffer-start buffer))))))
        ((octet-buffer-refillable-p buffer)
         (refill buffer)
         (octet-buffer-read-byte buffer))
        (t :eof)))

(defclass server-binary-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((octet-stream :reader octet-stream :initarg :octet-stream :initform (error "Octet stream must be supplied."))
   (octet-buffer :reader octet-buffer :initarg :octet-buffer :initform (error "Octet buffer must be supplied."))))

(defmethod trivial-gray-streams:stream-read-byte ((stream server-binary-input-stream))
  (with-interpreter-stream ((octet-stream stream))
    (octet-buffer-read-byte (octet-buffer stream))))

(defun make-server-binary-input-stream (stream)
  (make-instance 'server-binary-input-stream
                 :octet-stream stream
                 :octet-buffer (make-octet-buffer 4096)))

;;;;;;;;;;;;;;;;;;;

#|
don't dump fragments
for simplicity we flush every time we dump
|#

(defun utf8-dump (buffer remote-stream)
  (when (plusp (length buffer))
    (let ((dump-end (length buffer)))
      (when (>= (aref buffer (1- dump-end)) #x80)
        (decf dump-end)
        (loop for i from (1- (length buffer)) downto 0
              repeat 4
              until (>= (aref buffer i)
                        #b11000000)
              do (decf dump-end)))
      (when (plusp dump-end)
        (send-data buffer :end dump-end)
        (ecase remote-stream
          (:stdout
            (dump-to-stdout-c)
            (flush-stdout-c))
          (:stderr
            (dump-to-stderr-c)
            (flush-stderr-c)))
        (loop for i from dump-end below (length buffer)
              for j from 0
              do (setf (aref buffer j) (aref buffer i)))
        (setf (fill-pointer buffer) (- (length buffer) dump-end))))))

(defun output-buffer-write-byte (byte buffer remote-stream buffering)
  (check-type byte (unsigned-byte 8))
  ;; TODO not necessarily utf8
  (flet ((dump ()
           (utf8-dump buffer remote-stream)))
    (unless (vector-push byte buffer)
      (dump)
      (or (vector-push byte buffer) (error "This shouldn't happen.")))
    (ecase buffering
      (:fully-buffered)
      (:line-buffered (when (= byte #.(char-code #\Newline))
                        (dump)))
      (:unbuffered (dump))))
  byte)

(defclass server-binary-output-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((octet-stream :reader octet-stream :initarg :octet-stream :initform (error "Octet stream must be supplied."))
   (buffer :reader buffer :initarg :buffer :initform (error "Buffer must be supplied."))
   (remote-stream :reader remote-stream :initarg :remote-stream :initform (error "Buffer must be supplied."))
   (buffering :accessor buffering :initarg :buffering :initform :fully-buffered)))

(defun make-server-binary-output-stream (stream remote-stream buffering)
  (make-instance 'server-binary-output-stream
                 :octet-stream stream
                 :buffer (make-array *buffer-size*
                                     :element-type '(unsigned-byte 8)
                                     :fill-pointer 0)
                 :remote-stream remote-stream
                 :buffering buffering))

(defmethod trivial-gray-streams::stream-element-type ((stream server-binary-output-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-write-byte ((stream server-binary-output-stream) integer)
  (with-interpreter-stream ((octet-stream stream))
    (output-buffer-write-byte integer (buffer stream) (remote-stream stream) (buffering stream))))

(defmethod trivial-gray-streams:stream-finish-output ((stream server-binary-output-stream))
  (with-interpreter-stream ((octet-stream stream))
    (utf8-dump (buffer stream) (remote-stream stream)) ;flushing is here
    ))

;; I don't know why this is necessary, but otherwise (format *query-io* "~&foo~%) won't work
(defmethod sb-gray:stream-line-column ((s flexi-streams:flexi-input-stream))
  (declare (ignore s))
  nil)
