;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass editor ()
  ((arglist-display :initarg :arglist-display
                    :initform nil
                    :accessor arglist-display)
   (parsed :initform nil
           :accessor parsed))
  (:metaclass qt-class)
  (:qt-superclass "QTextEdit")
  (:slots ("changed()" text-changed)
          ("cursorChanged()" cursor-changed)))

(defmethod initialize-instance :after ((editor editor) &key parent)
  (new-instance editor parent)
  (#_setFont editor *default-qfont*)
  (connect editor "textChanged()"
           editor "changed()")
  (connect editor "cursorPositionChanged()"
           editor "cursorChanged()"))

(defun text-changed (editor)
  (with-signals-blocked (editor)
    (let* ((code (parse-lisp-string (#_toPlainText editor)))
           (cursor (#_textCursor editor))
           (position (#_position cursor)))
      (colorize code cursor)
      (display-arglist code position (arglist-display editor))
      (setf (parsed editor) code))))

(defun cursor-changed (editor)
  (display-arglist (parsed editor)
                   (#_position (#_textCursor editor))
                   (arglist-display editor)))
