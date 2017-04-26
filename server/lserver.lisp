;;;; lserver.lisp

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

(defparameter *body-buffer-size* 1024)
(defparameter *header-buffer-size* 3)
(defparameter *order-buffer-size* 3)
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
   (body-buffer :initform (make-array *body-buffer-size* :element-type '(unsigned-byte 8) :fill-pointer t) :reader body-buffer)
   (header-buffer :initform (make-array *header-buffer-size* :element-type '(unsigned-byte 8)) :reader header-buffer)
   (order-buffer :initform (make-array *order-buffer-size* :element-type '(unsigned-byte 8)) :reader order-buffer)))

(defun make-session-input-stream (stream)
  (make-instance 'session-input-stream :communication-stream stream))

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
  (let ((header-buffer (header-buffer stream))
        (body-buffer (body-buffer stream))
        (underlying-stream (communication-stream stream)))
    (unless (= (read-sequence header-buffer underlying-stream) *header-buffer-size*)
      (error 'corrupt-header :stream stream))
    (multiple-value-bind (type length) (decode-header header-buffer)
      (setf (fill-pointer body-buffer) length)
      (when (plusp length)
        (unless (= (read-sequence body-buffer underlying-stream) length)
          (error 'corrupt-body :stream stream)))
      (make-message :type type :body (decode-body type body-buffer)))))

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
                                   (2 . read-line)))

(defun order (stream command &optional data)
  (let ((length (if data (length data) 0))
        (order-buffer (order-buffer stream))
        (underlying-stream (communication-stream stream)))
    (setf (aref order-buffer 0) (or (car (rassoc command *order-type-codes*))
                                    (error "FAIL!!!"))
          (aref order-buffer 1) (ash length -8)
          (aref order-buffer 2) (logand length #b11111111))
    (write-sequence order-buffer underlying-stream)
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
