;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defun make-temporary-file ()
  #+(and sbcl (not win32))
  (sb-posix:mkstemp "/tmp/compilation.lispXXXXXX")
  #-(and sbcl (not win32))
  (uiop/stream:with-temporary-file (:keep t :pathname pathname)
    pathname))

(defun compile-form (form)
  (let ((temp (make-temporary-file)))
    (unwind-protect
         (progn
           (with-open-file (stream temp :direction :output
                                        :if-exists :supersede)
             (write-sequence form stream))
           (multiple-value-bind (truename warnings failure)
               (compile-file temp)
             (delete-file truename)
             (values warnings failure)))
      (delete-file temp))))
