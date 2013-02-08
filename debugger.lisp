;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass debugger ()
  ((condition :initform nil
              :initarg :condition)
   (selected-restart :initform nil)
   restarts)
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("restartSelected(int)" restart-selected)
          ("debugInSlime()" debug-in-slime)))

(defun debug-in-slime (window)
  (invoke-debugger (slot-value window 'condition)))

(defun restart-selected (window int)
  (setf (slot-value window 'selected-restart)
        (nth int (slot-value window 'restarts)))
  (#_accept window))

(defun add-restarts (window vbox restarts)
  (let ((button-group (#_new QButtonGroup)))
    (loop for restart in restarts
          for i from 0
          for button = (#_new QPushButton (string (restart-name restart)))
          do (#_addButton button-group button i)
          do (with-layout (hbox "QHBoxLayout" vbox)
               (add-widgets hbox button
                            (#_new QLabel (princ-to-string restart)))
               (#_addStretch hbox)))
    (connect button-group "buttonClicked(int)" window
             "restartSelected(int)")))

#+swank
(defmethod initialize-instance :after ((window debugger)
                                       &key parent condition)
  (new-instance window parent)
  (#_setWindowTitle window "Debugger")
  (let ((vbox (#_new QVBoxLayout window))
        (restarts (compute-restarts condition))
        (backtrace (make-instance 'list-widget
                                  :items (swank:backtrace 0 nil))))
    (setf (slot-value window 'restarts) restarts)
    (with-layout (hbox "QHBoxLayout" vbox)
      (let ((icon (#_new QLabel)))
        (#_setPixmap icon (#_pixmap (#_QIcon::fromTheme "dialog-error") 32 32))
        (add-widgets hbox icon)
        (output-text hbox (princ-to-string condition) :size 18)
        (#_addStretch hbox)))

    (add-restarts window vbox restarts)
    (add-widgets vbox backtrace))

  (connect window "rejected()"
           window "debugInSlime()"))
#+swank
(defun invoke-graphic-debugger (condition &optional debugger-hook)
  (declare (ignore debugger-hook))
  (handler-bind (;; (error (lambda (c)
                 ;;          (princ c)
                 ;;          (invoke-debugger condition)))
                 )
    (swank-backend:call-with-debugging-environment
     (lambda ()
       (let ((debugger (make-instance 'debugger :condition condition)))
         (#_exec debugger)
         (if (slot-value debugger 'selected-restart)
             (invoke-restart (slot-value debugger 'selected-restart))
             (invoke-debugger condition)))))))

(defmacro with-graphic-debugger (&body body)
  `(let ((*debugger-hook* 'invoke-graphic-debugger))
     ,@body))
