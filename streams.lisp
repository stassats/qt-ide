;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass repl-output-stream
    (fundamental-character-output-stream
     trivial-gray-stream-mixin)
  ((repl-window :initarg :repl-window
                :initform nil
                :accessor repl-window)))

(defun append-to-output (repl string)
  (emit-signal (output repl)
               "insertOutput(QString)" string))

(defmethod stream-write-char ((stream repl-output-stream) char)
  (append-to-output (repl-window stream) (string char)))

(defmethod stream-write-string
    ((stream repl-output-stream) string &optional (start 0) end)
  (append-to-output (repl-window stream) (subseq string start end)))

(defmethod stream-write-sequence
    ((stream repl-output-stream) string start end &key)
  (append-to-output (repl-window stream) (subseq string start end)))

(defmethod stream-terpri
    ((stream repl-output-stream))
  (append-to-output (repl-window stream) #.(string #\Newline)))
