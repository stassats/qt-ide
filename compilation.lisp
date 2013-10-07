;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defun make-temporary-file ()
  (sb-posix:mkstemp "/tmp/compilation.lispXXXXXX"))

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
