;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defun ide ()
  (let (*repl*
        *minibuffer*)
   (loop while
         (plusp (with-main-window (window (make-instance 'main-window))
                  (#_setCursorFlashTime *qapplication* 0))))))

(defmacro without-parent (function)
  `(lambda (x)
     (declare (ignore x))
     (,function)))

(defclass main-window (window)
  ((save-button :initform nil
                :accessor save-button)
   (editor :initform nil
              :accessor editor))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots
   ("openFile()" open-file-dialog)
   ("restart()"
    (without-parent
        (lambda ()
          (#_exit *qapplication* 1)))))
  (:default-initargs :title "IDE"))

(defmethod initialize-instance :after ((window main-window) &key)
  (let* ((central-widget (#_new QWidget window))
         (vbox (#_new QVBoxLayout central-widget))
         (*main-window* window)
         (toolbar (#_addToolBar window ""))
         (minibuffer (setf *minibuffer* (make-instance 'minibuffer)))
         (repl (setf *repl* (make-instance 'repl)))
         (editor (make-instance 'editor-tabs)))
    (#_setCentralWidget window central-widget)
    (add-widgets vbox editor
                 repl
                 ;; status-bar
                 minibuffer)
    (setf (editor window) editor)
    (add-qaction toolbar "Open file" window "openFile()"
                 :icon "document-open"
                 :key "Ctrl+o")
    (add-qaction toolbar "Restart"
                 window "restart()"
                 :icon "view-refresh")))

(defun parse-native-namestring (namestring)
  #+sbcl
  (sb-ext:native-namestring namestring)
  #+ccl
  (ccl:native-translated-namestring namestring)
  #-(or sbcl ccl)
  (parse-namestring namestring))

(defun open-file-dialog (window)
  (let ((file (car (file-dialog :parent window))))
    (when file
      (open-file (parse-native-namestring file) (editor window)))))

(defun display-arglist (code position minibuffer)
  (display (or (form-arglist position code) "")
           minibuffer))
