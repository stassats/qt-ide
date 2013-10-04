;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass editor ()
  ((minibuffer :initarg :minibuffer
                    :initform nil
                    :accessor minibuffer)
   (parsed :initform nil
           :accessor parsed)
   (repl :initarg :repl
         :initform nil
         :accessor repl))
  (:metaclass qt-class)
  (:qt-superclass "QTextEdit")
  (:slots ("changed()" text-changed)
          ("cursorChanged()" cursor-changed)
          ("compileForm()" compile-form)))

(defmethod initialize-instance :after ((editor editor) &key parent)
  (new-instance editor parent)
  (#_setFont editor *default-qfont*)
  (#_setAcceptRichText editor nil)
  ;(#_setCursorWidth editor 0)
  (connect editor "textChanged()"
           editor "changed()")
  (connect editor "cursorPositionChanged()"
           editor "cursorChanged()")
  (make-shortcut editor "Ctrl+f"
                 "compileForm()" :context :widget))

(defun text-changed (editor)
  (with-signals-blocked (editor)
    (let* ((code (parse-lisp-string (#_toPlainText editor)))
           (cursor (#_textCursor editor))
           (position (#_position cursor)))
      (colorize code cursor)
      (display-arglist code position (minibuffer editor))
      (setf (parsed editor) code))))

(defun cursor-changed (editor)
  (display-arglist (parsed editor)
                   (#_position (#_textCursor editor))
                   (minibuffer editor)))

(defun form-around-cursor (editor)
  (let ((form (find-form-around-position (#_position (#_textCursor editor))
                                         (parsed editor)))
        (text (#_toPlainText editor)))
    (when form
      (subseq text (p-start form)
              (p-end form)))))

(defun compile-form (editor)
  (eval-string (form-around-cursor editor)
               (repl editor)
               :result-to-minibuffer t))
