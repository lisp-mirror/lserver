;;;; lserver.lisp

(defpackage #:lserver (:use #:cl))
(in-package #:lserver)

;;; "lserver" goes here. Hacks and glory await!


(defparameter *server-stream-buffer-size* 1024)

(defmacro assuming-unbroken-stream ((stream) &rest body)
  (let ((c (gensym))
        (s (gensym))
        (l (gensym)))
    `(let* ((,s ,stream)
            (,l (lock ,s)))
       (bt:with-lock-held (,l)
         (handler-case
           (progn ,@body)
           (end-of-file (,c) (if (eq (stream-error-stream ,c) (communication-stream ,s))
                                 (error 'broken-pipe :stream ,s)
                                 (error ,c))))))))

(defun pop-saved-char (stream)
  (prog1
    (saved-char stream)
    (setf (saved-char stream) nil)))

(defstruct message type body)

(defparameter *incoming-buffer-size* 1024)
(defparameter *outgoing-buffer-size* 1024)
(defparameter *incoming-header-size* 3)
(defparameter *outgoing-header-size* 3)
(defparameter *type-codes* '((0 . eof)
                             (1 . character)
                             (2 . line)
                             (3 . line-part)
                             (4 . read-error)
                             (5 . flushed)))

(defclass session-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((saved-char :initform nil :accessor saved-char)
   (lock :initform (bt:make-lock "session input stream lock") :reader lock)
   (stream :initarg :communication-stream :reader communication-stream)
   (eofp :initform nil :accessor eofp)
   (incoming-buffer :initform (make-array *incoming-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer t) :reader incoming-buffer)
   (outgoing-buffer :initform (make-array *outgoing-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer t) :reader outgoing-buffer)
   (incoming-header :initform (make-array *incoming-header-size* :element-type '(unsigned-byte 8)) :reader incoming-header)
   (outgoing-header :initform (make-array *outgoing-header-size* :element-type '(unsigned-byte 8)) :reader outgoing-header)))

(defparameter *output-incoming-buffer-size* 4)
(defparameter *output-outgoing-buffer-size* 1024)

(defclass session-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((lock :initform (bt:make-lock "session output stream lock") :reader lock)
   (command :initarg :command :reader command)
   (stream :initarg :communication-stream :reader communication-stream)
   (incoming-buffer :initform (make-array *output-incoming-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer 0) :reader incoming-buffer)
   (outgoing-buffer :initform (make-array *output-outgoing-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer 0) :reader outgoing-buffer)
   (incoming-header :initform (make-array *incoming-header-size* :element-type '(unsigned-byte 8)) :reader incoming-header)
   (outgoing-header :initform (make-array *outgoing-header-size* :element-type '(unsigned-byte 8)) :reader outgoing-header)))

(defmethod trivial-gray-streams:stream-write-char ((stream session-output-stream) character)
  (let ((outgoing-buffer (outgoing-buffer stream)))
    (unless (vector-push-utf8 character outgoing-buffer)
      (order stream (command stream) outgoing-buffer)
      (setf (fill-pointer outgoing-buffer) 0)))
  character)

(defmethod trivial-gray-streams:stream-write-string ((stream session-output-stream) string &optional (start 0) end)
  (loop for i from start to (1- (or end (length string)))
        do (write-char (char string i) stream)
        finally (return string)))

(defmethod trivial-gray-streams:stream-force-output ((stream session-output-stream))
  (let ((outgoing-buffer (outgoing-buffer stream)))
    (order stream (command stream) outgoing-buffer)
    ;; add flush order
    (setf (fill-pointer outgoing-buffer) 0)))

(defmethod trivial-gray-streams:stream-finish-output ((stream session-output-stream))
  (let ((outgoing-buffer (outgoing-buffer stream)))
    (order stream (command stream) outgoing-buffer)
    ;; add flush order & feedback
    (setf (fill-pointer outgoing-buffer) 0)))

(defun make-session-input-stream (stream)
  (make-instance 'session-input-stream :communication-stream stream))

(defun make-session-output-stream (stream command)
  (make-instance 'session-output-stream :communication-stream stream :command command))

(defun decode-header (header)
  (values (or (cdr (assoc (aref header 0) *type-codes*))
              (error 'unknown-message-type-code :code (aref header 0)))
          (+ (ash (aref header 1) 8)
             (aref header 2))))

(defgeneric decode-body (type body))

(defmethod decode-body (type body)
  nil)

(defmethod decode-body ((type (eql 'line)) body)
  (babel:octets-to-string body))

(defmethod decode-body ((type (eql 'line-part)) body)
  (babel:octets-to-string body))

(defmethod decode-body ((type (eql 'character)) body)
  (char (babel:octets-to-string body) 0))

(defun read-message (stream)
  (let ((incoming-header (incoming-header stream))
        (incoming-buffer (incoming-buffer stream))
        (underlying-stream (communication-stream stream)))
    (unless (= (read-sequence incoming-header underlying-stream) *incoming-header-size*)
      (error 'corrupt-header :stream stream))
    (multiple-value-bind (type length) (decode-header incoming-header)
      (setf (fill-pointer incoming-buffer) length)
      (when (plusp length)
        (unless (= (read-sequence incoming-buffer underlying-stream) length)
          (error 'corrupt-body :stream stream)))
      (make-message :type type :body (decode-body type incoming-buffer)))))

(defmethod trivial-gray-streams:stream-read-char ((stream session-input-stream))
  (assuming-unbroken-stream (stream)
    (or (pop-saved-char stream)
        (get-new-char stream)))) 

(define-condition communication-error (stream-error) ())
(define-condition unknown-message-type-code (communication-error)
  ((code :initarg :expected)))
(define-condition broken-pipe (communication-error) ())
(define-condition corrupt-data (communication-error) ())
(define-condition corrupt-header (corrupt-data) ())
(define-condition corrupt-body (corrupt-data) ())

(define-condition type-mismatch (communication-error)
  ((expected :initarg :expected :initform t :reader expected)
   (received :initarg :received :initform t :reader received)))

(defparameter *order-type-codes* '((0 . exit)
                                   (1 . read-character)
                                   (2 . read-line)
                                   (3 . print-stdout)
                                   (4 . print-stderr)))

(defun order (stream command &optional data)
  (let ((length (if data (length data) 0))
        (outgoing-header (outgoing-header stream))
        (underlying-stream (communication-stream stream)))
    (setf (aref outgoing-header 0) (or (car (rassoc command *order-type-codes*))
                                    (error "FAIL!!!"))
          (aref outgoing-header 1) (ash length -8)
          (aref outgoing-header 2) (logand length #b11111111))
    (write-sequence outgoing-header underlying-stream)
    (when data
      (write-sequence data underlying-stream))
    (finish-output underlying-stream)))

(defun get-new-char (stream)
  (order stream 'read-character)
  (let ((response (read-message stream)))
    (case (message-type response)
      (character (message-body response))
      (eof :eof)
      (otherwise (error 'type-mismatch
                        :stream stream
                        :expected '(or character eof)
                        :received (message-type response))))))

(defun strcat (strings)
  (format nil "~{~A~}" strings))

(defmethod trivial-gray-streams:stream-read-line ((stream session-input-stream))
  (assuming-unbroken-stream (stream)
    (cond ((eql (saved-char stream) #\Newline)
           (setf (saved-char stream) nil)
           (values "" t))
          (t
           (order stream 'read-line)
           (loop with lines = (and (saved-char stream) (string (pop-saved-char stream)))
                 for response = (read-message stream)
                 for i from 1
                 do (print (message-type response) *terminal-io*)
                 do (case (message-type response)
                      (line-part (push (message-body response) lines))
                      (line (return (values (strcat (nreverse (cons (message-body response) lines)))
                                            nil)))
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

;; I'm trying to be smarter than the compiler
(defun vector-push-utf8 (character vector)
  (locally
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
                              (t (error "FAIL"))))))))
