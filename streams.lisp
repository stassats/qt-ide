;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass repl-stream ()
  ((window :initarg :window
           :initform nil
           :accessor window)
   (queue :initform (make-queue)
          :accessor queue)))

(defclass repl-output-stream
    (repl-stream fundamental-character-output-stream)
  ())

(defun append-to-output (stream string)
  (push-queue string (queue stream)))

(defmethod stream-write-char ((stream repl-output-stream) char)
  (append-to-output stream (string char)))

(defmethod stream-write-string
    ((stream repl-output-stream) string &optional (start 0) end)
  (append-to-output stream (subseq string start end)))

(defmethod stream-write-sequence
    ((stream repl-output-stream) string start end &key)
  (append-to-output stream (subseq string start end)))

(defmethod stream-terpri
    ((stream repl-output-stream))
  (append-to-output stream #.(string #\Newline)))

(defclass repl-input-stream
    (repl-stream fundamental-character-input-stream)
  ())

(defmethod stream-read-line ((stream repl-input-stream))
  (when (queue-empty-p (queue stream))
    (emit-signal (window stream) "queryInput()"))
  (values (with-output-to-string (str)
            (loop for char = (pop-queue (queue stream))
                  until (char= char #\Newline)
                  do
                  (write-char char str)))
          nil))

(defmethod stream-read-char ((stream repl-input-stream))
  (when (queue-empty-p (queue stream))
    (emit-signal (window stream) "queryInput()"))
  (values (pop-queue (queue stream))
          nil))

(defmethod stream-line-column ((stream repl-input-stream))
  nil)
