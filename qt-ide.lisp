;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defun ide ()
  (let ((*main-window* (make-instance 'main-window)))
    (unwind-protect
         (progn
           (#_show *main-window*)
           (#_exec *qapp*))
      (#_delete *main-window*))))

(defmacro without-parent (function)
  `(lambda (x)
     (declare (ignore x))
     (,function)))

(defclass main-window (window)
  ((save-button :initform nil
                :accessor save-button)
   (text-edit :initarg :text-edit
              :initform nil
              :accessor text-edit))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots
   ("openFile()" open-file)
   ("showRepl()"
    (without-parent repl)))
  (:default-initargs :title "IDE"))

(defmethod initialize-instance :after ((window main-window) &key)
  (let* ((central-widget (#_new QWidget window))
         (vbox (#_new QVBoxLayout central-widget))
         (*main-window* window)
         (toolbar (#_addToolBar window "Tracking"))
         (text-edit (make-instance 'editor)))
    (#_setCentralWidget window central-widget)
    (add-widgets vbox text-edit)
    (setf (text-edit window) text-edit)
    (add-qaction toolbar "Open file" window "openFile()"
                 :icon "document-open"
                 :key "Ctrl+o")
    (add-qaction toolbar "REPL" window "showRepl()"
                 :icon "utilities-terminal"
                 :key "Ctrl+r")))

(defun open-file (window)
  (let ((file (car (file-dialog :parent window))))
    (when file
      (let ((contents (alexandria:read-file-into-string file)))
        (#_setPlainText (text-edit window)
                        contents)))))
