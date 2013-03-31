;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *default-qfont* nil)
(defvar *default-font* "DejaVu Sans Mono 13")

(defclass window (qt-ui:window)
  ()
  (:metaclass qt-class))

(defmethod initialize-instance :before ((window window) &key)
  (ensure-qapp)
  (unless *default-qfont*
    (setf *default-qfont* (#_new QFont *default-font*))
    (#_setFixedPitch *default-qfont* t)))
