;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *default-qfont* nil)
(defvar *default-font* "DejaVu Sans Mono")
(defvar *default-font-size* 12)

(defvar *repl*)
(defvar *minibuffer*)

(defclass window (qt-ui:window)
  ()
  (:metaclass qt-class))

(defmethod initialize-instance :before ((window window) &key)
  (make-qapplication)
  (unless *default-qfont*
    (setf *default-qfont* (#_new QFont *default-font* *default-font-size*))
    (#_setFixedPitch *default-qfont* t)))
