;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *qapp* nil)
(defvar *main-window* nil)
(defvar *default-qfont* nil)
(defvar *default-font* "DejaVu Sans Mono 13")

(defun ide ()
  (unless *qapp*
    (setf *qapp* (make-qapplication)))
  (unless *default-qfont*
    (setf *default-qfont* (#_new QFont *default-font*)))
  (let ((*main-window* (make-instance 'main-window)))
    (unwind-protect
         (progn
           (#_show *main-window*)
           (#_exec *qapp*))
      (#_hide *main-window*))))

(defun exec-window (window &optional modal)
  (cond ((not *main-window*)
         (#_exec window))
        (modal
         (#_exec window))
        (t
         (#_show window))))

(defmacro without-parent (function)
  `(lambda (x)
     (declare (ignore x))
     (,function)))

(defclass main-window ()
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
    (without-parent repl))))

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
