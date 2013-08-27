;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defun find-form-around-position (position form)
  (flet ((inside (form)
           (< (p-start form)
              position
              (if (eq (p-error form) *end-of-file*)
                  (1+ (p-end form))
                  (p-end form)))))
    (cond ((not (inside form))
           nil)
          ((p-list-p form)
           (or
            (loop for (x . rest) on (p-list-items form)
                  if (and (p-list-p x)
                          (not (null (p-list-items x)))
                          (find-form-around-position position x))
                  return it
                  while (consp rest))
            form))
          (t
           form))))

(defun form-arglist (position form)
  (let* ((form (find-form-around-position position form))
         (list (and (p-list-p form)
                    (p-list-items form)))
         (symbol (and (consp list)
                      (typep (car list) 'p-symbol)
                      (resolve-p-symbol (car list)))))
    (when symbol
      (let ((*print-case* :downcase))
        (format nil "(~a ~{~a~^ ~})" symbol (get-arglist symbol))))))

(defun get-arglist (symbol)
  (cond ((and (fboundp symbol)
              (not (macro-function symbol)))
         (sb-introspect:function-lambda-list symbol))))
