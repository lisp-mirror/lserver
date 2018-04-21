;;;; communication.lisp

(in-package #:lserver-communication)

(define-condition communication-error (stream-error) ())
(define-condition unknown-message-type-code (communication-error)
  ((code :initarg :expected)))
(define-condition broken-pipe (communication-error) ())
(define-condition corrupt-data (communication-error) ())
(define-condition corrupt-header (communication-error) ())

(define-condition type-mismatch (communication-error)
  ((expected :initarg :expected :initform t :reader expected)
   (received :initarg :received :initform t :reader received)))

(define-condition client-error (error)
  ((errno :initarg :errno :initform nil :reader errno)))
(define-condition flush-error (client-error) ())
(define-condition write-error (client-error) ())

(defparameter *buffer-size* 1024)
(defparameter *header-size* 3)
(defparameter *small-buffer-size* 4)

(defstruct message type data)

(defclass session-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((saved-char :initform nil :accessor saved-char)
   (wrapped-stream :reader communication-stream)
   (eofp :initform nil :accessor eofp)
   (buffer :initform (make-array *buffer-size* :element-type '(unsigned-byte 8) :fill-pointer t) :reader buffer)
   (incoming-header :initform (make-array *header-size* :element-type '(unsigned-byte 8)) :reader incoming-header)
   (outgoing-header :initform (make-array *header-size* :element-type '(unsigned-byte 8)) :reader outgoing-header)))

(defun make-session-input-stream (stream)
  (let ((s (make-instance 'session-input-stream)))
    (setf (slot-value s 'wrapped-stream) stream)
    s))

(defclass session-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((command :reader command)
   (wrapped-stream :reader communication-stream)
   (incoming-buffer :initform (make-array *small-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer 0) :reader incoming-buffer)
   (buffer :initform (make-array *buffer-size* :element-type '(unsigned-byte 8) :fill-pointer 0) :reader buffer)
   (incoming-header-buffer :initform (make-array *header-size* :element-type '(unsigned-byte 8)) :reader incoming-header)
   (outgoing-header-buffer :initform (make-array *header-size* :element-type '(unsigned-byte 8)) :reader outgoing-header)
   (line-column :initform 0 :accessor line-column)))

(defun make-session-output-stream (stream command)
  (let ((s (make-instance 'session-output-stream)))
    (setf (slot-value s 'command) command
          (slot-value s 'wrapped-stream) stream)
    s))

(defmacro with-stream ((stream) &rest body)
  (let ((c (gensym))
        (s (gensym)))
    `(let* ((,s ,stream))
       (handler-case
         (progn ,@body)
         (end-of-file (,c) (if (eq (stream-error-stream ,c) (communication-stream ,s))
                               (error 'broken-pipe :stream ,s)
                               (error ,c)))))))

(defun pop-saved-char (stream)
  (prog1
    (saved-char stream)
    (setf (saved-char stream) nil)))

;;;;;;;; Basic messaging ;;;;;;;;;;

(declaim (inline decode-header))
(defun decode-header (header)
  (values (case (aref header 0)
            (0 'eof)
            (1 'character)
            (2 'line)
            (3 'line-part)
            (4 'read-error)
            (5 'flush-error)
            (6 'written)
            (7 'write-error)
            (8 'text)
            (9 'text-part)
            (10 'int)
            (otherwise (error 'unknown-message-type-code :code (aref header 0))))
          (+ (ash (aref header 1) 8)
             (aref header 2))))

(declaim (inline decode-utf8-char))
(defun decode-utf8-char (b0 b1 b2 b3)
  (declare (type (unsigned-byte 8) b0 b1 b2 b3)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (cond ((not (logbitp 7 b0)) (code-char (ldb (byte 7 0) b0)))
        ((= (ldb (byte 3 5) b0) #b110)
         (code-char (+ (ash (ldb (byte 5 0) b0) 6)
                       (ldb (byte 6 0) b1))))
        ((= (ldb (byte 4 4) b0) #b1110)
         (code-char (+ (ash (ldb (byte 4 0) b0) 16)
                       (ash (ldb (byte 6 0) b1) 8)
                       (ldb (byte 6 0) b2))))
        ((= (ldb (byte 4 4) b0) #b11110)
         (code-char (+ (ash (ldb (byte 2 0) b0) 24)
                       (ash (ldb (byte 6 0) b1) 16)
                       (ash (ldb (byte 6 0) b2) 8)
                       (ldb (byte 6 0) b3))))))

(declaim (inline decode-int))
(defun decode-int (b0 b1 b2 b3)
  (declare (type (unsigned-byte 8) b0 b1 b2 b3)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (+ b0
     (ash b1 8)
     (ash b2 16)
     (ash b3 32)))

(declaim (inline decode-data))
(defun decode-data (message-type raw-data)
  (ecase message-type
    ((eof written) nil)
    ((line line-part text text-part) (trivial-utf-8:utf-8-bytes-to-string raw-data))
    (character (decode-utf8-char (aref raw-data 0) (aref raw-data 1) (aref raw-data 2) (aref raw-data 3)))
    ((read-error write-error flush-error int) (decode-int (aref raw-data 0) (aref raw-data 1) (aref raw-data 2) (aref raw-data 3)))))

(defun read-message (stream)
  (let ((incoming-header (incoming-header stream))
        (buffer (buffer stream))
        (underlying-stream (communication-stream stream)))
    (unless (= (read-sequence incoming-header underlying-stream) *header-size*)
      (error 'corrupt-header :stream stream))
    (multiple-value-bind (type length) (decode-header incoming-header)
      #+nil (print type #.*terminal-io*)
      #+nil (print length #.*terminal-io*)
      (setf (fill-pointer buffer) length)
      (when (plusp length)
        (unless (= (read-sequence buffer underlying-stream) length)
          (error 'corrupt-data :stream stream)))
      (make-message :type type :data (decode-data type buffer)))))

(declaim (inline buffer-from-integer))
(defun buffer-from-integer (integer stream)
  (dotimes (i 4)
    (setf (aref (buffer stream) i) (ldb (byte 8 (* 8 i)) integer)))
  (setf (fill-pointer (buffer stream)) 4))

(declaim (inline empty-buffer))
(defun empty-buffer (stream)
  (setf (fill-pointer (buffer stream)) 0))

(declaim (inline encode-data))
(defun encode-data (order-type data stream)
  "Digest data and put in into stream's buffer, return the length of the buffer."
  (ecase order-type
    (exit (buffer-from-integer data stream))
    ((read-character read-line) (empty-buffer stream))
    ((print-stdout print-stderr) (length (buffer stream)))
    ((cwd program-name lisp-args) 0)))

(declaim (inline encode-header))
(defun encode-header (header order data-length)
  (let ((code (ecase order
                (exit 0)
                (read-character 1)
                (read-line 2)
                (print-stdout 3)
                (print-stderr 4)
                (cwd 5)
                (program-name 6)
                (lisp-args 7))))
    (setf (aref header 0) code
          (aref header 1) (ldb (byte 8 0) data-length)
          (aref header 2) (ldb (byte 8 8) data-length))))

(defun order (stream command &optional data)
  #+nil (format #.*terminal-io* "~A ~A~%" command data)
  (let ((length (encode-data command data stream))
        (outgoing-header (outgoing-header stream))
        (underlying-stream (communication-stream stream)))
    (encode-header outgoing-header command length)
    (write-sequence outgoing-header underlying-stream)
    (when (plusp length)
      (write-sequence (buffer stream) underlying-stream))
    (finish-output underlying-stream)
    #+nil (format #.*terminal-io* "bye~%")
    ))

(defun read-int (stream)
  (let ((msg (read-message stream)))
    (case (message-type msg)
      (int (message-data msg))
      (otherwise (error 'type-mismatch
                        :stream stream
                        :expected 'int
                        :received (message-type msg))))))

(defun read-text (stream)
  (loop with parts = nil
        for msg = (read-message stream)
        do (case (message-type msg)
             (text-part (push (message-data msg) parts))
             ;; it's silly, but I don't feel like consing
             (text (if parts
                       (return (strcat (nreverse (cons (message-data msg) parts))))
                       (return (message-data msg))))
             (otherwise (error 'type-mismatch
                               :stream stream
                               :expected '(or text text-part)
                               :received (message-type msg))))))

(defun query-cwd (stream)
  (with-stream (stream)
    (order stream 'cwd)
    (read-text stream)))

(defun query-program-name (stream)
  (with-stream (stream)
    (order stream 'program-name)
    (read-text stream)))

(defun query-lisp-args (stream)
  (with-stream (stream)
    (order stream 'lisp-args)
    (let ((argc (read-int stream)))
      (loop repeat argc collecting (read-text stream)))))


;;;;;;;; INPUT ;;;;;;;;

(defun get-new-char (stream)
  (order stream 'read-character)
  (let ((response (read-message stream)))
    (case (message-type response)
      (character (message-data response))
      (eof :eof)
      (otherwise (error 'type-mismatch
                        :stream stream
                        :expected '(or character eof)
                        :received (message-type response))))))

(defmethod trivial-gray-streams:stream-read-char ((stream session-input-stream))
  (with-stream (stream)
    (or (pop-saved-char stream)
        (get-new-char stream)))) 

(defun strcat (strings)
  (format nil "~{~A~}" strings))

(defmethod trivial-gray-streams:stream-read-line ((stream session-input-stream))
  (with-stream (stream)
    (cond ((eql (saved-char stream) #\Newline)
           (setf (saved-char stream) nil)
           (values "" t))
          (t
           (order stream 'read-line)
           (loop with lines = (and (saved-char stream) (list (string (pop-saved-char stream))))
                 for response = (read-message stream)
                 for i from 1
                 do (case (message-type response)
                      (line-part (push (message-data response) lines))
                      ;; it's silly, but I don't feel like consing
                      (line (if lines
                                (return (values (strcat (nreverse (cons (message-data response) lines)))
                                                nil))
                                (return (values (message-data response)
                                                nil))))
                      (eof (return (values (strcat (nreverse lines))
                                           t)))
                      (otherwise (error 'type-mismatch
                                        :stream stream
                                        :expected '(or line line-part eof)
                                        :received (message-type response)))))))))

(defmethod trivial-gray-streams:stream-unread-char ((stream session-input-stream) character)
  (setf (saved-char stream) character)
  nil)

(defmethod trivial-gray-streams:stream-peek-char ((stream session-input-stream))
  (or (saved-char stream) (get-new-char stream)))


(defmethod trivial-gray-streams:stream-line-column ((stream session-input-stream))
  nil)

;;;;;;;; OUTPUT ;;;;;;;

;; I'm trying to be smarter than the compiler
(declaim (inline vector-push-utf8))
(defun vector-push-utf8 (character vector)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((code (char-code character))
        (room (- (array-dimension vector 0) (length vector))))
    (cond ((>= room 4) (cond ((< code #x80) (vector-push code vector))
                             ((< code #x800) (vector-push (+ #b11000000 (ldb (byte 5 6) code)) vector)
                                             (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                             ((< code #x10000) (vector-push (+ #b11100000 (ldb (byte 4 12) code)) vector)
                                               (vector-push (+ #b10000000 (ldb (byte 6 6) code)) vector)
                                               (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                             ((< code #x200000) (vector-push (+ #b11110000 (ldb (byte 3 18) code)) vector)
                                                (vector-push (+ #b10000000 (ldb (byte 6 12) code)) vector)
                                                (vector-push (+ #b10000000 (ldb (byte 6 6) code)) vector)
                                                (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                             (t (error "FAIL"))))
          ((= room 3) (cond ((< code #x80) (vector-push code vector))
                            ((< code #x800) (vector-push (+ #b11000000 (ldb (byte 5 6) code)) vector)
                                            (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                            ((< code #x10000) (vector-push (+ #b11100000 (ldb (byte 4 12) code)) vector)
                                              (vector-push (+ #b10000000 (ldb (byte 6 6) code)) vector)
                                              (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                            ((< code #x200000) nil)
                            (t (error "FAIL"))))
          ((= room 2) (cond ((< code #x80) (vector-push code vector))
                            ((< code #x800) (vector-push (+ #b11000000 (ldb (byte 5 6) code)) vector)
                                            (vector-push (+ #b10000000 (ldb (byte 6 0) code)) vector))
                            ((< code #x200000) nil)
                            (t (error "FAIL"))))
          ((= room 1) (cond ((< code #x80) (vector-push code vector))
                            ((< code #x200000) nil)
                            (t (error "FAIL"))))
          ((= room 0) (cond ((< code #x200000) nil)
                            (t (error "FAIL")))))))

(defmethod trivial-gray-streams:stream-line-column ((stream session-output-stream))
  (line-column stream))

(defmethod trivial-gray-streams:stream-start-line-p ((stream session-output-stream))
  (zerop (line-column stream)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream session-output-stream))
  (unless (zerop (line-column stream))
    (terpri stream)
    t))

(defun write-buffer (stream)
  (let ((buffer (buffer stream)))
    (order stream (command stream) buffer)
    (let ((response (read-message stream)))
      (case (message-type response)
        (written (message-data response))
        (write-error (error 'write-error :errno (message-data response)))
        (flush-error (error 'flush-error :errno (message-data response)))
        (otherwise (error 'type-mismatch
                          :stream stream
                          :expected '(or character eof)
                          :received (message-type response)))))
    (setf (fill-pointer buffer) 0)))

(defmethod trivial-gray-streams:stream-write-char ((stream session-output-stream) character)
  (if (char= character #\Newline)
      (setf (line-column stream) 0)
      (incf (line-column stream)))
  (let ((buffer (buffer stream)))
    (unless (vector-push-utf8 character buffer)
      (with-stream (stream)
        (write-buffer stream))
      (vector-push-utf8 character buffer)))
  character)

(defmethod trivial-gray-streams:stream-write-string ((stream session-output-stream) string &optional (start 0) end)
  (loop for i fixnum from start to (1- (the fixnum (or end (length string))))
        do (write-char (char (the string string) i) stream)
        finally (return string)))

(defmethod trivial-gray-streams:stream-force-output ((stream session-output-stream))
  (with-stream (stream)
    (write-buffer stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream session-output-stream))
  (with-stream (stream)
    (write-buffer stream)))
