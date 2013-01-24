;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defun repl (&key class modal text
                  fold-results)
  (exec-window (make-instance 'repl) modal))

(defclass repl ()
  ((input :initarg :input
          :initform nil
          :accessor input)
   (scene :initarg :scene
           :initform nil
           :accessor scene)
   (layout :initarg :layout
           :initform nil
           :accessor layout)
   (last-output-position :initarg :last-output-position
                         :initform 0
                         :accessor last-output-position)
   (view :initarg :view
         :initform nil
         :accessor view))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("evaluate()" evaluate)))

(defclass repl-input ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("keyPressEvent" key-press-event)
             ("keyReleaseEvent" key-release-event))
  (:slots)
  (:signals ("returnPressed()")))

(defmethod key-press-event ((widget repl-input) event)
  (let ((key (#_key event)))
    (cond ((or (= key (primitive-value (#_Qt::Key_Return)))
               (= key (primitive-value (#_Qt::Key_Enter))))
           (#_accept event)
           (emit-signal widget "returnPressed()"))
          (t
           (stop-overriding)))))

(defmethod key-release-event ((widget repl-input) event)
  (let ((key (#_key event)))
    (if (or (= key (primitive-value (#_Qt::Key_Return)))
            (= key (primitive-value (#_Qt::Key_Enter))))
        (#_ensureVisible widget)
        (stop-overriding))))

(defmethod initialize-instance :after ((widget repl-input) &key scene)
  (new-instance widget "")
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)
                                             (#_Qt::TextEditable)))
  (#_addItem scene widget)
  (#_setFont widget *default-qfont*))

(defmethod initialize-instance :after ((window repl) &key parent)
  (new-instance window parent)
  (#_setWindowTitle window "REPL")
  (let* ((scene (#_new QGraphicsScene window))
         (view (#_new QGraphicsView scene window))
         (vbox (#_new QVBoxLayout window))
         (input (make-instance 'repl-input :scene scene)))
    (#_setAlignment view (enum-or (#_Qt::AlignLeft) (#_Qt::AlignTop)))
    (add-widgets vbox view)
    (setf (input window) input
          (scene window) scene
          (view window) view)
    (#_setFocus input)
    (connect input "returnPressed()"
             window "evaluate()")))

(defun evaluate-string (string)
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*debug-io* (make-two-way-stream *debug-io* stream))
          (*query-io* (make-two-way-stream *query-io* stream)))
       (format stream "~&=> ~{~a~^, ~}"
               (with-graphic-debugger
                (multiple-value-list (eval (read-from-string string))))))))

(defun evaluate (window)
  (with-slots (input scene last-output-position view) window
    (let* ((text (#_toPlainText input))
           (text (#_addText scene (format nil "~a~%~a" text
                                          (evaluate-string text))
                            *default-qfont*)))
      (#_setTextInteractionFlags text (enum-or (#_Qt::TextSelectableByMouse)
                                               (#_Qt::TextSelectableByKeyboard)))
      (#_setPos text 0 last-output-position)
      (incf last-output-position (#_height (#_boundingRect text)))
      (#_setPlainText input "")
      (#_setPos input 0 last-output-position))))
