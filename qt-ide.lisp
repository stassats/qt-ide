;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defun ide ()
  (loop while
        (plusp (with-main-window (window (make-instance 'main-window))
                 (#_setCursorFlashTime *qapplication* 0)))))

(defmacro without-parent (function)
  `(lambda (x)
     (declare (ignore x))
     (,function)))

(defclass main-window (window)
  ((save-button :initform nil
                :accessor save-button)
   (text-edit :initform nil
              :accessor text-edit)
   (minibuffer :initform nil
               :accessor minibuffer)
   (repl :initarg :repl
         :initform nil
         :accessor repl))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots
   ("openFile()" open-file)
   ("showRepl()"
    (without-parent repl))
   ("restart()"
    (without-parent
        (lambda ()
          (#_exit *qapplication* 1)))))
  (:default-initargs :title "IDE"))

(defmethod initialize-instance :after ((window main-window) &key)
  (let* ((central-widget (#_new QWidget window))
         (vbox (#_new QVBoxLayout central-widget))
         (*main-window* window)
         (toolbar (#_addToolBar window "Tracking"))
         (minibuffer (make-instance 'minibuffer))
         (status-bar (#_new QStatusBar))
         (repl (make-instance 'repl
                              :minibuffer minibuffer))
         (text-edit (make-instance 'editor
                                   :minibuffer minibuffer
                                   :repl repl)))
    (#_setCentralWidget window central-widget)
    (add-widgets vbox text-edit
                 repl
                 ;; status-bar
                 minibuffer)
    (setf (text-edit window) text-edit
          (minibuffer window) minibuffer
          (repl window) repl)
    (add-qaction toolbar "Open file" window "openFile()"
                 :icon "document-open"
                 :key "Ctrl+o")
    (add-qaction toolbar "Restart"
                 window "restart()"
                 :icon "view-refresh")))

(defun open-file (window)
  (let ((file (car (file-dialog :parent window))))
    (when file
      (let ((contents (alexandria:read-file-into-string file)))
        (#_setPlainText (text-edit window)
                        contents)))))

(defun display-arglist (code position minibuffer)
  (display (or (form-arglist position code) "")
           minibuffer))
