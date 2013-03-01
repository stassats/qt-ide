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
  (with-slots (current-output last-output-position
               scroll-bar)
      repl
    (unless current-output
      (setf current-output (add-text-to-repl "" repl)))
    (let* ((current-text (#_toPlainText current-output))
           (document (#_document current-output))
           (current-height (#_rheight (#_size document)))
           (height
             (progn (#_setPlainText current-output
                                    (concatenate 'string
                                                 current-text
                                                 string))
                    (#_rheight (#_size (#_document current-output)))))
           (new-height (- height current-height)))
      (#_setMaximum scroll-bar
                    (+ (#_maximum scroll-bar)
                       (ceiling new-height)))
      (incf last-output-position new-height))))

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
