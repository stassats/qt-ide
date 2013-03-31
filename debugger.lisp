;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass debugger (window)
  ((context :initarg :context
            :initform nil
            :accessor context) 
   (restarts :initform nil
             :accessor restarts)
   (query-stream :initarg :query-stream
                 :initform nil
                 :accessor query-stream))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("restartSelected(int)" restart-selected)
          ("debugInSlime()" debug-in-slime))
  (:default-initargs :title "Debugger"))

(defmethod initialize-instance :after ((window debugger)
                                       &key context repl)
  (let* ((vbox (#_new QVBoxLayout window))
         (condition (debugger-context-condition context))
         (restarts (call-in-channel context
                                    (lambda () (compute-restarts condition))))
         (backtrace (make-instance 'list-widget
                                   :items
                                   (call-in-channel
                                    context
                                    (lambda ()
                                      (swank:backtrace 0 nil))))))
    (setf (restarts window) restarts
          (query-stream window) (query-stream repl))
    (with-layout (hbox "QHBoxLayout" vbox)
      (let ((icon (#_new QLabel)))
        (#_setPixmap icon (#_pixmap (#_QIcon::fromTheme "dialog-error") 32 32))
        (add-widgets hbox icon)
        (output-text hbox (princ-to-string condition) :size 18)
        (#_addStretch hbox)))
    (add-restarts window vbox restarts)
    (add-widgets vbox backtrace)))

(defstruct (debugger-context
            (:include channel))
  condition)

(defun query-input (window)
  (with-slots (query-stream) window
    (let* ((output-queue (queue
                          (two-way-stream-output-stream
                           query-stream)))
           (input-queue (queue
                          (two-way-stream-input-stream
                           query-stream)))
           (prompt (concatenate-output-queue output-queue))
           (input (input-line-dialog :parent window :prompt prompt)))
      (loop for char across input
            do
            (push-queue char input-queue))
      (push-queue #\Newline input-queue))))

(defun restart-selected (window int)
  (submit-to-channel
   (context window)
   (lambda ()
     (let ((*query-io* (query-stream window)))
       (invoke-restart-interactively
        (nth int (restarts window))))))
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

(defun invoke-graphic-debugger (parent condition &optional debugger-hook)
  (declare (ignore debugger-hook))
  (let ((context (make-debugger-context :condition condition)))
    (push-queue
     (lambda ()
       (exec-window (make-instance 'debugger :context context
                                             :repl parent)))
     (debugger-queue parent))
    (emit-signal parent "invokeDebugger()")
    (swank-backend:call-with-debugging-environment
     (lambda ()
       (channel-loop context)))))

(defmacro with-graphic-debugger ((parent) &body body)
  `(let* ((*debugger-hook*
            (lambda (condition debugger-hook)
              (invoke-graphic-debugger ,parent condition debugger-hook))))
     ,@body))
