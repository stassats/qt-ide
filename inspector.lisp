;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass inspector (navigable-viewer)
  ()
  (:metaclass qt-class)
  (:default-initargs
   :page-class (find-class 'inspector-page)))

(defclass inspector-page (viewer-page)
  ()
  (:metaclass qt-class))

(defun inspector (object)
  (exec-window (make-instance 'inspector :object object)))

(defmethod view-object ((page inspector-page) object layout)
  (inspect-object object layout))

(defgeneric inspect-object (object layout))

;;;

(defmethod inspect-object (object layout)
  (output-text layout
               (format nil "An object of type ~s" (type-of object)) :size 18)
  (#_addStretch layout))
