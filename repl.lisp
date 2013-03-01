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
   (item-count :initarg :item-count
               :initform -1
               :accessor item-count)
   (view :initarg :view
         :initform nil
         :accessor view)
   (scroll-bar :initarg :scroll-bar
               :initform nil
               :accessor scroll-bar)
   (output :initarg :output
                   :initform nil
                   :accessor output) 
   (output-stream :initarg :output-stream
                  :initform nil
                  :accessor output-stream))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("evaluate()" evaluate)
   ("history(bool)" choose-history)
   ("insertOutput(QString)" insert-output))
  (:default-initargs :title "REPL"))

(defmethod initialize-instance :after ((window repl) &key)
  (let* ((scene (#_new QGraphicsScene window))
         (view (#_new QGraphicsView scene window))
         (vbox (#_new QVBoxLayout window))
         (input (make-instance 'repl-input))
         (package-indicator (#_addText scene "" *default-qfont*))
         (layout (#_new QGraphicsLinearLayout (#_Qt::Vertical)))
         (hlayout (#_new QGraphicsLinearLayout))
         (main-widget (#_new QGraphicsWidget)))
    (#_setAlignment view (enum-or (#_Qt::AlignLeft) (#_Qt::AlignTop)))
    (#_setContentsMargins layout 0 0 0 0)
    (#_setSpacing layout 0)
    (#_setSpacing hlayout 0)
    (add-widgets vbox view)
    (#_setLayout main-widget layout)
    (#_addItem scene main-widget)
    (#_addItem layout hlayout)
    (setf (input window) input
          (package-indicator window) package-indicator
          (scene window) scene
          (view window) view
          (output-stream window)
          (make-instance 'repl-output-stream
                         :repl-window window)
          (scroll-bar window) (#_verticalScrollBar view)
          (layout window) layout)
    (#_setDefaultTextColor package-indicator
                           (or *package-indicator-color*
                               (setf *package-indicator-color*
                                     (#_new QColor "#a020f0"))))
    (#_setDocumentMargin (#_document package-indicator) 0)
    (add-text-to-layout hlayout package-indicator)
    (add-text-to-layout hlayout input)
    (update-input window)
    (#_setFocus input)
    (connect input "returnPressed()"
             window "evaluate()")
    (connect input "history(bool)"
             window "history(bool)")))

(defun add-text-to-layout (layout item &key position)
  (let ((widget (#_new QGraphicsWidget))
        (document (#_document item)))
    (#_setParentItem item widget)
    (#_setPreferredSize widget (#_size document))
    (#_setSizePolicy widget (#_new QSizePolicy
                                   (#_QSizePolicy::Fixed)
                                   (#_QSizePolicy::Fixed)))
    (if position
        (#_insertItem layout position widget)
        (#_addItem layout widget))
    (connect document "contentsChanged()"
             (lambda ()
               (#_setPreferredSize widget (#_size document))))))

(defun insert-item (repl item)
  (with-slots (input scene scroll-bar
               layout item-count) repl
    (add-text-to-layout layout item
                        :position (incf item-count))
    (let* ((max (+ (#_maximum scroll-bar)
                   (ceiling (#_rheight (#_size (#_document item)))))))
      (#_setMaximum scroll-bar max)
      (#_setValue scroll-bar max))))

;;;

(defclass text-item ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("contextMenuEvent" context-menu-event)))

(defmethod initialize-instance :after ((widget text-item) &key (text "")
                                                               editable)
  (new-instance widget text)
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)
                                             (if editable
                                                 (#_Qt::TextEditable)
                                                 0)))
  (#_setDocumentMargin (#_document widget) 1)
  (#_setFont widget *default-qfont*))

(defmethod context-menu-event ((widget text-item) event)
  (let ((menu (context-menu widget)))
    (when menu
      (#_exec menu (#_screenPos event)))))

;;;

(defclass repl-input (text-item)
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
   ("history(bool)"))
  (:default-initargs :editable t))

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

;;;

(defclass result-presentation (text-item)
  ((value :initarg :value
          :initform nil
          :accessor value))
  (:metaclass qt-class)
  (:slots ("inspect()" (lambda (x)
                         (inspector (value x))))))

(defmethod initialize-instance :after ((widget result-presentation)
                                       &key)
  (#_setDefaultTextColor widget (#_new QColor "#ff0000")))

(defmethod context-menu ((widget result-presentation))
  (let ((menu (#_new QMenu)))
    (add-qaction menu "Inspect" widget "inspect()")
    menu))
;;;

(defclass repl-output (text-item)
  ((cursor :initarg :cursor
           :initform nil
           :accessor cursor)
   (used :initarg :used
         :initform nil
         :accessor used))
  (:metaclass qt-class)
  (:signals ("insertOutput(QString)")))

(defmethod initialize-instance :after ((widget repl-output) &key
                                                              repl)
  (setf (cursor widget)
        (#_new QTextCursor (#_document widget)))
  (connect widget "insertOutput(QString)" repl "insertOutput(QString)"))

;;;

(defun update-input (repl)
  (with-slots (input package-indicator scroll-bar)
      repl
    (#_setPlainText input "")
    (#_setPlainText package-indicator
                    (format nil "~a> " (short-package-name *package*)))
    (#_setValue scroll-bar (#_maximum scroll-bar))))

(defun evaluate-string (repl string)
  (with-slots (output-stream output)
      repl
    (when (or (null output)
              (used output))
      (setf output (make-instance 'repl-output :repl repl)))
    (let ((*standard-output* output-stream)
          (*error-output* output-stream)
          ;;(*debug-io* (make-two-way-stream *debug-io* output-stream))
          (*query-io* (make-two-way-stream *query-io* output-stream)))
      (with-graphic-debugger
        (multiple-value-list
         (eval (read-from-string string)))))))

(defun adjust-history (new-input)
  (unless (equal new-input (car *repl-history*))
    (let ((stripped (string-trim #(#\Space #\Newline #\Tab #\Return) new-input)))
      (setf *repl-history*
            (cons stripped
                  (remove stripped *repl-history* :test #'equal))))))

(defun add-text-to-repl (text repl)
  (insert-item repl (make-instance 'text-item :text text)))

(defun add-result-to-repl (value repl)
  (insert-item repl (make-instance 'result-presentation
                                   :value value
                                   :text (prin1-to-string value))))

(defun insert-output (repl string)
  (with-slots (output scroll-bar) repl
    (let* ((output (output repl))
           (cursor (cursor output))
           (document (#_document output)))
      (unless (used output)
        (insert-item repl output)
        (setf (used output) t))
      (let* ((old-height (#_rheight (#_size document))))
        (#_movePosition cursor (#_QTextCursor::End))
        (#_insertText cursor string)
        (let* ((new-height (- (#_rheight (#_size document))
                              old-height))
               (max (+ (#_maximum scroll-bar)
                       (ceiling new-height))))
          (#_setMaximum scroll-bar max)
          (#_setValue scroll-bar max))))))

(defun add-results (results repl)
  (cond ((null results)
         (add-text-to-repl "; No values" repl))
        (t
         (loop for result in results
               do (add-result-to-repl result repl)))))

(defun evaluate (repl)
  (with-slots (input package-indicator) repl
    (let ((string-to-eval (#_toPlainText input)))
      (add-text-to-repl
       (format nil "~a> ~a"
               (short-package-name *package*)
               string-to-eval)
       repl)
      (add-results (evaluate-string repl string-to-eval) repl)
      (update-input repl)
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
