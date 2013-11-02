;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass completer (list-widget)
  ((query-package :initarg :package
                  :initform nil
                  :accessor query-package)
   (query :initarg :query
          :initform nil
          :accessor query))
  (:metaclass qt-class)
  (:override ("keyPressEvent" key-press-event)
             ("focusOutEvent" focus-out-event))
  (:default-initargs :description #'print-symbol-cased))

(defmethod initialize-instance :after ((widget completer) &key query package)
  (setf (items widget)
        (find-completions query package))
  (setf (current-index widget) 0))

(defun completion-selected (completer)
  (let* ((query (query completer))
         (editor (#_parent completer))
         (cursor (#_textCursor editor))
         (selected (car (selected-items completer))))
    (select-form query cursor)
    (with-signals-blocked (editor)
      (#_removeSelectedText cursor))
    (#_insertText cursor (print-symbol-cased selected))))

(defmethod key-press-event ((widget completer) event)
  (let ((key (#_key event))
        (parent (#_parent widget)))
    (when (= key (enum-value (#_Qt::Key_Escape)))
      (#_close widget)
      (#_setFocus parent))
    (when (or (= key (enum-value (#_Qt::Key_Tab)))
              (= key (enum-value (#_Qt::Key_Enter)))
              (= key (enum-value (#_Qt::Key_Space))))
      (completion-selected widget)
      (#_close widget)
      (#_setFocus parent)
      (return-from key-press-event))
    (stop-overriding)))

(defun find-completions (query package)
  (let* ((p-symbol-name (p-symbol-name query))
         (p-package (p-symbol-package query))
         (package (typecase p-package
                    (string
                     (or (find-package p-package)
                         package))
                    (package
                     p-package)
                    (t
                     package))))
    (sort (loop for symbol being the symbols in package
                for name = (symbol-name symbol)
                for score = (search p-symbol-name name)
                when (eql score 0)
                collect symbol)
          #'<
          :key (lambda (x) (length (symbol-name x))))))

(defmethod focus-out-event ((widget completer) event)
  (let ((reason (#_reason event)))
    (unless (enum= reason (#_Qt::ActiveWindowFocusReason))
      (#_close widget))))

(defun print-symbol-cased (symbol)
  (let ((name (symbol-name symbol)))
    (flet ((case-p (test)
             (loop for char across name
                   always (or (not (both-case-p char))
                              (funcall test char)))))
      (case (readtable-case *readtable*)
        (:upcase (if (case-p #'upper-case-p)
                     (string-downcase name)
                     (format nil "|~a|" name)))
        (:downcase (if (case-p #'lower-case-p)
                       name
                       (format nil "|~a|" name)))
        (:preserve name)
        (:invert (map 'string
                      (lambda (char) (if (upper-case-p char)
                                         (char-downcase char)
                                         (char-upcase char)))
                      name))))))
