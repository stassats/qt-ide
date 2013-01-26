;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *repl-history* nil)
(defvar *package-indicator-color* nil)

(defun repl (&key modal)
  (let ((*package* *package*)
        (* *)
        (** **)
        (*** ***)
        (/ /)
        (// //)
        (/// ///)
        (+ +)
        (++ ++)
        (+++ +++)
        (- -))
    (exec-window (make-instance 'repl) modal)))

(defun short-package-name (package)
  (let* ((name (package-name package))
         (shortest (length name)))
    (loop for nickname in (package-nicknames package)
          when (< (length nickname) shortest)
          do (setf name nickname)) 
    name))

(defclass repl ()
  ((input :initarg :input
          :initform nil
          :accessor input)
   (package-indicator :initarg :package-indicator
                      :initform nil
                      :accessor package-indicator)
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
   ("evaluate()" evaluate)
   ("history(bool)" choose-history)))

(defclass repl-input ()
  ((history-index :initarg :history-index
                  :initform -1
                  :accessor history-index)
   (current-input :initarg :current-input
                  :initform nil
                  :accessor current-input))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("keyPressEvent" key-press-event))
  (:signals
   ("returnPressed()")
   ("history(bool)")))

(defmethod key-press-event ((widget repl-input) event)
  (let ((key (#_key event)))
    (cond ((or (= key (primitive-value (#_Qt::Key_Return)))
               (= key (primitive-value (#_Qt::Key_Enter))))
           (#_accept event)
           (emit-signal widget "returnPressed()"))
          ((= key (primitive-value (#_Qt::Key_Up)))
           (#_accept event)
           (emit-signal widget "history(bool)" t))
          ((= key (primitive-value (#_Qt::Key_Down)))
           (#_accept event)
           (emit-signal widget "history(bool)" nil))
          (t
           (setf (history-index widget) -1)
           (stop-overriding)))))

(defmethod initialize-instance :after ((widget repl-input) &key scene)
  (new-instance widget "")
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)
                                             (#_Qt::TextEditable)))
  (#_addItem scene widget)
  (#_setFont widget *default-qfont*))

(defun update-input (y window)
  (with-slots (input package-indicator)
      window
    (#_setPlainText input "")
    (#_setPlainText package-indicator
                    (format nil "~a>" (short-package-name *package*)))
    (#_setPos input (#_rwidth (#_size (#_document package-indicator)))
              y)
    (#_setY package-indicator y)
    (#_ensureVisible input)))

(defmethod initialize-instance :after ((window repl) &key parent)
  (new-instance window parent)
  (#_setWindowTitle window "REPL")
  (let* ((scene (#_new QGraphicsScene window))
         (view (#_new QGraphicsView scene window))
         (vbox (#_new QVBoxLayout window))
         (input (make-instance 'repl-input :scene scene))
         (package-indicator (#_addText scene "" *default-qfont*)))
    (#_setAlignment view (enum-or (#_Qt::AlignLeft) (#_Qt::AlignTop)))
    (add-widgets vbox view)
    (setf (input window) input
          (package-indicator window) package-indicator
          (scene window) scene
          (view window) view)
    (#_setDefaultTextColor package-indicator
                       (or *package-indicator-color*
                           (setf *package-indicator-color*
                                 (#_new QColor "#a020f0"))))
    (update-input 0 window)
    (#_setFocus input)
    (connect input "returnPressed()"
             window "evaluate()")
    (connect input "history(bool)"
             window "history(bool)")))

(defun evaluate-string (string)
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*debug-io* (make-two-way-stream *debug-io* stream))
          (*query-io* (make-two-way-stream *query-io* stream)))
       (format stream "~&=> ~{~a~^, ~}"
               (with-graphic-debugger
                (multiple-value-list (eval (read-from-string string))))))))

(defun adjust-history (new-input)
  (unless (equal new-input (car *repl-history*))
    (let ((stripped (string-trim #(#\Space #\Newline #\Tab #\Return) new-input)))
      (setf *repl-history*
            (cons stripped
                  (remove stripped *repl-history* :test #'equal))))))

(defun evaluate (window)
  (with-slots (input scene last-output-position view
               package-indicator)
      window
    (let* ((string-to-eval (#_toPlainText input))
           (text (#_addText scene (format nil "~a> ~a~%~a" (short-package-name *package*)
                                          string-to-eval
                                          (evaluate-string string-to-eval))
                            *default-qfont*))
           (height (#_rheight (#_size (#_document text))))
           (scroll-bar (#_verticalScrollBar view)))
      (#_setTextInteractionFlags text (enum-or (#_Qt::TextSelectableByMouse)
                                               (#_Qt::TextSelectableByKeyboard)))
      (#_setY text last-output-position)
      (#_setMaximum scroll-bar (+ (#_maximum scroll-bar) (ceiling height)))
      (update-input (incf last-output-position height)
                    window)
      (adjust-history string-to-eval)
      (setf (history-index input) -1))))

(defun choose-history (window previous-p)
  (with-slots (input scene last-output-position view) window
    (let* ((text (#_toPlainText input))
           (current-index (history-index input))
           (next-index (min
                        (max (+ current-index
                                (if previous-p
                                    1
                                    -1))
                             -1)
                        (1- (length *repl-history*)))))
      (when (= current-index -1)
        (setf (current-input input) text))
      (setf (history-index input) next-index )
      (#_setPlainText input (if (= next-index -1)
                                (current-input input)
                                (nth next-index *repl-history*)))
      (let ((cursor (#_textCursor input)))
        (#_movePosition cursor (#_QTextCursor::End))
        (#_setTextCursor input cursor)))))
