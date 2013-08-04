;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defun form-arglist (form)
  (when (and (consp form)
             (typep (car form) 'p-symbol))
    (let ((symbol (resolve-p-symbol (car form))))
      (when symbol
        (let ((*print-case* :downcase))
         (format nil "(~a ~{~a~^ ~})" symbol (get-arglist symbol)))))))

(defun get-arglist (symbol)
  (sb-introspect:function-lambda-list symbol))
