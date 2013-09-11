;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass minibuffer ()
  ((output :initform nil
           :accessor output)
   (input :initform nil
          :accessor input)
   (input-label :initform nil
                :accessor input-label)
   (input-widget :initform nil
                 :accessor input-widget)
   (focused :initform nil
            :accessor focused))
  (:metaclass qt-class)
  (:qt-superclass "QStackedWidget")
  (:slots ("openEditor()" minibuffer-editor)
          ("exitEditor()" minibuffer-exit)
          ("execute()" (lambda (widget)
                         (execute widget "M-x")))
          ("focusChanged(QWidget*,QWidget*)" focus-changed)))

(defmethod initialize-instance :after ((widget minibuffer) &key parent)
  (new-instance widget parent)
  (#_setFont widget *default-qfont*)
  (#_setSizePolicy widget (#_QSizePolicy::Expanding) (#_QSizePolicy::Maximum))
  (#_setContentsMargins widget 0 0 0 0)
  (let* ((output (#_new QLabel))
         (input-widget (#_new QWidget))
         (input-label (#_new QLabel))
         (input (#_new QLineEdit))
         (input-layout (#_new QHBoxLayout)))
    (setf (output widget) output
          (input widget) input
          (input-label widget) input-label
          (input-widget widget) input-widget)
    (#_setLayout input-widget input-layout)
    (add-widgets input-layout input-label input)
    (#_setSizePolicy input-widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setContentsMargins input-layout 0 0 0 0)
    (add-widgets widget output input-widget)
    (#_setCurrentWidget widget output)
    (make-shortcut widget "Alt+x" "execute()")
    (make-shortcut widget "Ctrl+g" "exitEditor()"
                   :context :widget-with-children)
    (connect *qapplication* "focusChanged(QWidget*,QWidget*)"
             widget "focusChanged(QWidget*,QWidget*)")))

(defun display (text minibuffer)
  (#_setText (output minibuffer) text))

(defun execute (minibuffer &optional (label ""))
  (with-slots (input-widget input-label
               input) minibuffer
    (#_setCurrentWidget minibuffer input-widget)
    (#_setText input-label label)
    (#_setFocus input)))

(defun minibuffer-exit (minibuffer)
  (#_setCurrentWidget minibuffer (output minibuffer))
  (#_setFocus (focused minibuffer)))

(defun focus-changed (minibuffer old new)
  (when (and (eql new (input minibuffer))
             (not (null-qobject-p old)))
    (setf (focused minibuffer) old)))
