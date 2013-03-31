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

;;;

(defvar *end-channel* (gensym "END-CHANNEL"))

(defstruct (channel)
  (eval-queue (make-queue))
  (result-queue (make-queue)))

(defun call-in-channel (channel function)
  (push-queue function (channel-eval-queue channel))
  (values-list (pop-queue (channel-result-queue channel))))

(defun channel-loop (channel)
  (let ((eval-queue (channel-eval-queue channel))
        (result-queue (channel-result-queue channel)))
    (loop with result
          with success
          for function = (pop-queue eval-queue)
          until (eq function *end-channel*)
          do
          (setf success nil)
          (unwind-protect
               (setf result
                     (multiple-value-list (funcall function))
                     success t)
            (unless success
              (push-queue nil result-queue)))
          (push-queue result result-queue))))
