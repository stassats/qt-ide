;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defun repl (&key class modal text
                  fold-results)
  (exec-window (make-instance 'repl) modal))

(defclass repl ()
  ((input :initarg :input
          :initform nil
          :accessor input)
   (output :initarg :output
           :initform nil
           :accessor output))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("evaluate()" evaluate)))

(defmethod initialize-instance :after ((window repl) &key parent)
  (new-instance window parent)
  (#_setWindowTitle window "REPL")
  (let ((output (#_new QTextEdit))
        (input (#_new QLineEdit))
        (vbox (#_new QVBoxLayout window)))
    (add-widgets vbox output input)
    (#_setFocus input)
    (setf (input window) input
          (output window) output)
    (connect input "returnPressed()"
             window "evaluate()")))

(defun evaluate-string (string)
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*debug-io* (make-two-way-stream *debug-io* stream))
          (*query-io* (make-two-way-stream *query-io* stream)))
       (format stream "~&=> ~{~a~^, ~}"
               (multiple-value-list (eval (read-from-string string)))))))

(defun evaluate (window)
  (with-slots (input output) window
    (let ((text (#_text input)))
     (#_append output
               (format nil "~a~%~a" text
                       (evaluate-string text))))
    (#_clear input)))
