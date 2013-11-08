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

(defmethod initialize-instance :after ((widget completer) &key)
  (find-completions widget)
  (setf (current-index widget) 0))

(defun reinit-completions (completer)
  (let* ((editor (#_parent completer))
         (position (#_position (#_textCursor editor)))
         (symbol (find-symbol-at-position position (parsed editor))))
    (setf (query completer) symbol)
    (find-completions completer)
    (setf (current-index completer) 0)))

(defun completion-selected (completer space)
  (let* ((query (query completer))
         (editor (#_parent completer))
         (cursor (#_textCursor editor))
         (selected (car (selected-items completer))))
    (select-form query cursor)
    (with-signals-blocked (editor)
      (#_removeSelectedText cursor))
    (#_insertText cursor (format nil "~a~@[ ~]"
                                (print-string-cased selected)
                                space))))

(defmethod key-press-event ((widget completer) event)
  (let ((key (#_key event))
        (parent (#_parent widget)))
    (when (= key (enum-value (#_Qt::Key_Escape)))
      (#_close widget)
      (#_setFocus parent))
    (let ((space-p (= key (enum-value (#_Qt::Key_Space)))))
      (when (or space-p
                (= key (enum-value (#_Qt::Key_Tab)))
                (= key (enum-value (#_Qt::Key_Return))))
        (completion-selected widget space-p)
        (#_close widget)
        (#_setFocus parent)
        (return-from key-press-event)))
    (#_notify *qapplication* parent event)
    (reinit-completions widget)))

(defun do-find-completions (query package)
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

(defun find-completions (completer)
  (with-slots (query query-package) completer
    (setf (items completer)
          (when query
            (do-find-completions query query-package)))))

(defmethod focus-out-event ((widget completer) event)
  (let ((reason (#_reason event)))
    (unless (enum= reason (#_Qt::ActiveWindowFocusReason))
      (#_close widget))))
