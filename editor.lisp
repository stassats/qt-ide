;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass editor ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTextEdit")
  (:slots ("changed()" text-changed)))

(defmethod initialize-instance :after ((editor editor) &key parent)
  (new-instance editor parent)
  (#_setFont editor *default-qfont*)
  (connect editor "textChanged()"
           editor "changed()"))

(defun text-changed (editor)
  (with-signals-blocked (editor)
   (colorize (#_toPlainText editor) (#_textCursor editor))))
