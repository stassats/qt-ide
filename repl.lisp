;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *repl-history* nil)
(defvar *package-indicator-color* nil)

(defun repl ()
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
    (exec-window (make-instance 'repl))))

(defun short-package-name (package)
  (let* ((name (package-name package))
         (shortest (length name)))
    (loop for nickname in (package-nicknames package)
          when (< (length nickname) shortest)
          do (setf name nickname)) 
    name))

(defclass repl (window)
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
   ("history(bool)" choose-history))
  (:default-initargs :title "REPL"))

(defclass repl-input ()
  ((history-index :initarg :history-index
                  :initform -1
                  :accessor history-index)
   (current-input :initarg :current-input
                  :initform nil
                  :accessor current-input))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("keyPressEvent" key-press-event)
             ("paint" graphics-item-paint))
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

(defgeneric graphics-item-paint (item painter option widget))

(defmethod graphics-item-paint ((item repl-input) painter option widget)
  (#_setState option (enum-andc (#_state option)
                                (#_QStyle::State_Selected)
                                (#_QStyle::State_HasFocus)))
  (stop-overriding))

(defmethod initialize-instance :after ((widget repl-input) &key scene)
  (new-instance widget "")
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)
                                             (#_Qt::TextEditable)))
  (#_setDocumentMargin (#_document widget) 1)
  (#_addItem scene widget)
  (#_setFont widget *default-qfont*))

(defclass result-presentation ()
  ((value :initarg :value
          :initform nil
          :accessor value))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("contextMenuEvent" context-menu-event)
   ;; ("paint" graphics-item-paint)
   )
  (:slots ("inspect()" (lambda (x)
                         (inspector (value x))))))

(defmethod initialize-instance :after ((widget result-presentation)
                                       &key text scene)
  (new-instance widget text)
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)))
  (#_setDocumentMargin (#_document widget) 1)
  (#_addItem scene widget)
  (#_setFont widget *default-qfont*)
  (#_setDefaultTextColor widget (#_new QColor "#ff0000")))

(defgeneric context-menu-event (widget event))

(defmethod context-menu-event ((widget result-presentation) event)
  (let ((menu (#_new QMenu)))
    (add-qaction menu "Inspect"
                 widget
                 "inspect()")
    (#_exec menu (#_screenPos event))))

;;;

(defun update-input (window)
  (with-slots (input last-output-position package-indicator)
      window
    (#_setPlainText input "")
    (#_setPlainText package-indicator
                    (format nil "~a> " (short-package-name *package*)))
    (#_setPos input (- (#_rwidth (#_size (#_document package-indicator)))
                       2) ;; margins
              last-output-position)
    (#_setY package-indicator last-output-position)
    (#_ensureVisible input)))

(defmethod initialize-instance :after ((window repl) &key)
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
    (#_setDocumentMargin (#_document package-indicator) 1)
    (update-input window)
    (#_setFocus input)
    (connect input "returnPressed()"
             window "evaluate()")
    (connect input "history(bool)"
             window "history(bool)")))

(defun evaluate-string (string)
  (let* (results
         (output
           (with-output-to-string (stream)
             (let ((*standard-output* stream)
                   (*error-output* stream)
                   (*debug-io* (make-two-way-stream *debug-io* stream))
                   (*query-io* (make-two-way-stream *query-io* stream)))
               (with-graphic-debugger
                 (setf results (multiple-value-list
                                (eval (read-from-string string)))))))))
    (values results output)))

(defun adjust-history (new-input)
  (unless (equal new-input (car *repl-history*))
    (let ((stripped (string-trim #(#\Space #\Newline #\Tab #\Return) new-input)))
      (setf *repl-history*
            (cons stripped
                  (remove stripped *repl-history* :test #'equal))))))

(defun add-text-to-repl (text window)
  (with-slots (input scene last-output-position view)
      window
    (let* ((text (#_addText scene text *default-qfont*))
           (height (progn
                     (#_setDocumentMargin (#_document text) 1)
                     (#_rheight (#_size (#_document text)))))
           (scroll-bar (#_verticalScrollBar view)))
      (#_setTextInteractionFlags text (enum-or (#_Qt::TextSelectableByMouse)
                                               (#_Qt::TextSelectableByKeyboard)))
      (#_setY text last-output-position)
      (#_setMaximum scroll-bar (+ (#_maximum scroll-bar) (ceiling height)))
      (incf last-output-position height))))

(defun add-result-to-repl (value window)
  (with-slots (input scene last-output-position view)
      window
    (let* ((text (make-instance 'result-presentation
                                :scene scene
                                :value value
                                :text (prin1-to-string value)))
           (height (progn
                     (#_setDocumentMargin (#_document text) 1)
                     (#_rheight (#_size (#_document text)))))
           (scroll-bar (#_verticalScrollBar view)))
      (#_setY text last-output-position)
      (#_setMaximum scroll-bar (+ (#_maximum scroll-bar) (ceiling height)))
      (incf last-output-position height))))

(defun add-results (results window)
  (cond ((null results)
         (add-text-to-repl "; No values" window)) ;
        (t
         (loop for result in results
               do (add-result-to-repl result window)))))

(defun evaluate (window)
  (with-slots (input scene last-output-position view
               package-indicator)
      window
    (let ((string-to-eval (#_toPlainText input)))
      (multiple-value-bind (values output)
          (evaluate-string string-to-eval)
        (add-text-to-repl
         (format nil "~a> ~a~@[~%~a~]"
                 (short-package-name *package*)
                 string-to-eval
                 (if (equal output "")
                     nil
                     output))
         window)
        (add-results values window)
        (update-input window)
        (adjust-history string-to-eval)
        (setf (history-index input) -1)))))

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
