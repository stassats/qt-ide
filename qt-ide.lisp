;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *qapp* nil)
(defvar *main-window* nil)

(defun ide ()
  (unless *qapp*
    (setf *qapp* (make-qapplication)))
  (let ((*main-window* (make-instance 'main-window)))
    (unwind-protect
         (progn
           (#_show *main-window*)
           (#_exec *qapp*))
      (#_hide *main-window*))))

(defclass main-window ()
  ((save-button :initform nil
                :accessor save-button)
   (text-edit :initarg :text-edit
              :initform nil
              :accessor text-edit))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots
   ("openFile()" open-file)))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (let* ((central-widget (#_new QWidget window))
         (vbox (#_new QVBoxLayout central-widget))
         (*main-window* window)
         (toolbar (#_addToolBar window "Tracking"))
         (text-edit (#_new QTextEdit)))
    (#_setWindowTitle window "IDE")
    (#_QIcon::setThemeName "oxygen")
    (#_setCentralWidget window central-widget)
    (add-widgets vbox text-edit)
    (setf (text-edit window) text-edit)
    (add-qaction toolbar "Open file" window "openFile()"
                 :icon "document-open"
                 :key "Ctrl+o")))

(defun open-file (window)
  (let ((file (car (file-dialog :parent window))))
    (when file
      (let ((contents (alexandria:read-file-into-string file)))
        (#_setPlainText (text-edit window)
                        contents)))))
