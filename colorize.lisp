;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *def-color* "#a020f0")
(defvar *string-color* "#8b2252")
(defvar *comment-color* "#b22222")
(defvar *flash-color* "#fbf826")

(defvar *colors* '(*string-color* *def-color*
                   *comment-color* *flash-color*))

(defun init-colors ()
  (loop for color in *colors*
        for value = (symbol-value color)
        when (stringp value)
        do (setf (symbol-value color)
                 (#_new QBrush (#_new QColor value)))))

(defun colorize (code cursor)
  (init-colors)
  (#_select cursor (#_QTextCursor::Document))
  (#_setCharFormat cursor (#_new QTextCharFormat))
  (colorize-code code cursor))

;;; 

(defvar *defining-forms*
  '(defun defmacro defmethod defgeneric
    defsetf defvar defparameter
    defconstant deftype defclass defstruct
    defpackage define-condition
    define-modify-macro define-compiler-macro
    define-symbol-macro define-setf-expander
    define-method-combination))

(defun defining-form-p (form)
  (let ((operator (resolve-form-operator form)))
    (member operator *defining-forms* :test #'eq)))

(defun select-region (start end cursor)
  (#_setPosition cursor start)
  (#_movePosition cursor
                  (#_QTextCursor::Right)
                  (#_QTextCursor::KeepAnchor)
                  (- end start)))

(defun select-form (form cursor)
  (select-region (p-start form) (p-end form) cursor))

(defun colorize-background-form (form cursor color)
  (with-objects ((text-format (#_new QTextCharFormat)))
    (select-form form cursor)
    (#_setBackground text-format color)
    (#_mergeCharFormat cursor text-format)))

(defun clear-background (form cursor)
  (with-objects ((text-format (#_new QTextCharFormat)))
    (select-form form cursor)
    ;; Set to the default, so that it will be merged over the existing one
    (#_setBackground text-format (#_background text-format))
    (#_mergeCharFormat cursor text-format)))

(defun colorize-form (form cursor color)
  (with-objects ((text-format (#_new QTextCharFormat)))
    (select-form form cursor)
    (#_setForeground text-format color)
    (#_setCharFormat cursor text-format)))

(defun colorize-conditional (cond cursor)
  (cond ((eval-conditional cond)
         (colorize-code (p-conditional-code cond) cursor))
        (t
         (colorize-form cond cursor *comment-color*)
         (loop for code in (p-conditional-code cond)
               do
               (colorize-form code cursor *comment-color*)))))

(defun map-cons (function cons)
  (loop for (car . cdr) on cons
        do (funcall function car)
           (cond ((consp cdr))
                 ((null cdr)
                  (return))
                 (t
                  (funcall function cdr)
                  (return)))))

(defun colorize-code (code cursor)
  (labels ((map-code (code)
             (typecase code
               (p-list
                (let ((list (p-list-items code)))
                  (when (defining-form-p code)
                    (colorize-form (p-nth 0 code) cursor *def-color*)
                    (pop list))
                  (map-cons #'map-code list)))
               (p-string
                (colorize-form code cursor *string-color*))
               (p-comment
                (colorize-form code cursor *comment-color*))
               (p-conditional
                (colorize-conditional code cursor)))))
    (map-cons #'map-code code)))

