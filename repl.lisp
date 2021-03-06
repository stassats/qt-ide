;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *repl-history* nil)
(defvar *package-indicator-color* nil)
(defvar *repl-channel* nil)

(defun short-package-name (package)
  (let* ((name (package-name package))
         (shortest (length name)))
    (loop for nickname in (package-nicknames package)
          when (< (length nickname) shortest)
          do (setf name nickname))
    name))

(defclass repl-scene ()
  ((repl-window :initarg :parent
                :initform nil
                :accessor repl-window))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsScene")
  (:override ("keyPressEvent" key-press-event)))

(defmethod initialize-instance :after ((scene repl-scene) &key parent)
  (new-instance scene parent))

(defmethod key-press-event ((widget repl-scene) event)
  (let ((input (input (repl-window widget))))
    (#_setFocus input)
    (stop-overriding)))

(defclass repl ()
  ((eval-channel :initform nil
                 :accessor eval-channel)
   (input :initform nil
          :accessor input)
   (current-package :initarg :current-package
                    :initform nil
                    :accessor current-package) 
   (package-indicator :initform nil
                      :accessor package-indicator)
   (scene :initform nil
          :accessor scene)
   (things-to-move :initarg :things-to-move
                   :initform nil
                   :accessor things-to-move) 
   (item-position :initform 0
                  :accessor item-position)
   (view :initform nil
         :accessor view)
   (output :initform (make-instance 'repl-output)
           :accessor output)
   (output-stream :initform nil
                  :accessor output-stream)
   (timer :initform nil
          :accessor timer)
   (debugger-queue :initform (make-queue)
                   :accessor debugger-queue)
   (query-stream :initarg :query-stream
                 :initform nil
                 :accessor query-stream))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsView")
  (:slots
   ("evaluate()" evaluate)
   ("history(bool)" choose-history)
   ("insertOutput()" insert-output)
   ("insertResults()" insert-results)
   ("displayResultsMinibuffer()" display-results-minibuffer)
   ("invokeDebugger()" start-debugger)
   ("makeInputVisible(QRectF)" make-input-visible)
   ("queryInput()" query-input)
   ("displayArglist()" display-repl-arglist))
  (:signals ("insertResults()")
            ("displayResultsMinibuffer()")
            ("invokeDebugger()")
            ("queryInput()")))

(defmethod initialize-instance :after ((repl repl) &key)
  (new-instance repl)
  (let* ((scene (make-instance 'repl-scene :parent repl))
         (input (make-instance 'repl-input))
         (package-indicator (make-instance 'package-indicator))
         (timer (#_new QTimer repl)))
    (#_setScene repl scene)
    (#_setAlignment repl (enum-or (#_Qt::AlignLeft) (#_Qt::AlignTop)))
    (#_setItemIndexMethod scene (#_QGraphicsScene::NoIndex))
    (if (and *repl-channel*
             (channel-alive-p *repl-channel*))
        (flush-channel *repl-channel*)
        (setf *repl-channel* (make-channel-thread "qt-repl")))
    (setf (eval-channel repl) *repl-channel*
          (timer repl) timer
          (current-package repl)
          (call-in-channel (eval-channel repl)
                           (lambda () *package*))
          (input repl) input
          (package-indicator repl) package-indicator
          (scene repl) scene
          (output-stream repl)
          (make-instance 'repl-output-stream :window repl)
          (query-stream repl)
          (make-two-way-stream
           (make-instance 'repl-input-stream :window repl)
           (make-instance 'repl-output-stream :window repl)))
    (update-input repl)
    (#_addItem scene input)
    (#_addItem scene package-indicator)
    (#_setFocus input)
    (connect input "returnPressed()"
             repl "evaluate()")
    (connect input "history(bool)"
             repl "history(bool)")
    (connect input "arglistChanged()"
             repl "displayArglist()")
    (connect scene "sceneRectChanged(QRectF)"
             repl "makeInputVisible(QRectF)")
    (connect repl "insertResults()"
             repl "insertResults()")
    (connect repl "displayResultsMinibuffer()"
             repl "displayResultsMinibuffer()")
    (connect repl "invokeDebugger()"
             repl "invokeDebugger()")
    (connect repl "queryInput()"
             repl "queryInput()")
    (connect timer "timeout()" repl "insertOutput()")
    (#_start timer 30)))

(defun adjust-items-after-output (repl amount move)
  (with-slots (package-indicator input
               item-position things-to-move) repl
    (when move
      (loop for thing in things-to-move
            do
            (#_moveBy thing 0 amount)))
    (#_moveBy input 0 amount)
    (#_moveBy package-indicator 0 amount)
    (incf item-position amount)))

(defun add-text-to-repl (repl item &key position)
  (with-slots (scene item-position) repl
    (let* ((document (#_document item))
           (height (#_height (#_size document))))
      (#_addItem scene item)
      (#_setY item (or position
                       item-position))
      (adjust-items-after-output repl height
                                 (typep item 'repl-output))))
  item)

;;;

(defclass text-item ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("contextMenuEvent" context-menu-event)
             ("paint" graphics-item-paint)))

(defmethod initialize-instance :after ((widget text-item) &key (text "")
                                                               editable)
  (new-instance widget text)
  (#_setTextInteractionFlags widget (enum-or (#_Qt::TextSelectableByMouse)
                                             (#_Qt::TextSelectableByKeyboard)
                                             (if editable
                                                 (#_Qt::TextEditable)
                                                 0)))
  (unless editable
    (#_setUndoRedoEnabled (#_document widget) nil))
  (#_setDocumentMargin (#_document widget) 1)
  (#_setFont widget *default-qfont*))

(defmethod context-menu-event ((widget text-item) event)
  (let ((menu (context-menu widget)))
    (when menu
      (#_exec menu (#_screenPos event)))))

(defgeneric graphics-item-paint (item painter option widget))

(defmethod graphics-item-paint :before ((item text-item) painter option widget)
  (#_setState option (enum-andc (#_state option)
                                (#_QStyle::State_Selected)
                                (#_QStyle::State_HasFocus))))

(defmethod graphics-item-paint ((item text-item) painter option widget)
  (stop-overriding))

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
             ("mousePressEvent" mouse-press-event))
  (:signals
   ("returnPressed()")
   ("history(bool)")
   ("arglistChanged()"))
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
           (call-next-qmethod)))
    (emit-signal widget "arglistChanged()")))

(defmethod mouse-press-event ((widget repl-input) event)
  (call-next-qmethod)
  (emit-signal widget "arglistChanged()"))

;;;

(defclass result-presentation (text-item)
  ((value :initarg :value
          :initform nil
          :accessor value)
   (hovered :initarg :hovered
            :initform nil
            :accessor hovered))
  (:metaclass qt-class)
  (:override ("hoverEnterEvent" hover-enter-event)
             ("hoverLeaveEvent" hover-leave-event))
  (:slots ("inspect()" (lambda (x)
                         (inspector (value x))))))

(defmethod initialize-instance :after ((widget result-presentation)
                                       &key)
  (#_setDefaultTextColor widget (#_new QColor "#ff0000")))


(defgeneric hover-enter-event (widget event))
(defgeneric hover-leave-event (widget event))

(defmethod hover-enter-event ((widget result-presentation) event)
  (setf (hovered widget) t)
  (#_update widget))

(defmethod hover-leave-event ((widget result-presentation) event)
  (setf (hovered widget) nil)
  (#_update widget))

(defmethod context-menu ((widget result-presentation))
  (let ((menu (#_new QMenu)))
    (add-qaction menu "Inspect" widget "inspect()")
    menu))

(defmethod graphics-item-paint ((item result-presentation) painter option widget)
  (cond ((hovered item)
         (call-next-qmethod)
         (#_drawRoundedRect painter (#_boundingRect item) 2 2))
        (t
         (stop-overriding))))

;;;

(defclass repl-output (text-item)
  ((cursor :initarg :cursor
           :initform nil
           :accessor cursor)
   (used :initarg :used
         :initform nil
         :accessor used)
   (place :initarg :place
          :initform nil
          :accessor place))
  (:metaclass qt-class))

(defmethod initialize-instance :after ((widget repl-output) &key)
  (setf (cursor widget)
        (#_new QTextCursor (#_document widget))))
;;;

(defclass package-indicator (text-item)
  ()
  (:metaclass qt-class))

(defmethod initialize-instance :after ((widget package-indicator) &key)
  (#_setDefaultTextColor widget
                         (or *package-indicator-color*
                             (setf *package-indicator-color*
                                   (#_new QColor "#a020f0")))))

;;;

(defun update-input (repl)
  (with-slots (input package-indicator) repl
    (#_setPlainText input "")
    (#_setPlainText package-indicator
                    (format nil "~a> "
                            (short-package-name (current-package repl))))
    (#_setX input (- (#_width (#_size (#_document package-indicator)))
                     2)) ;; margins
    (#_setVisible input t)
    (#_setVisible package-indicator t)
    (#_setFocus input)))

(defun make-input-visible (repl rect)
  (#_centerOn repl (#_bottomLeft rect)))

(defmacro with-eval-restarts (&body body)
  `(block nil
     (restart-case
         (progn ,@body)
       (abort ()
         :report "Return to toplevel"
         (return)))))

(defun evaluate-string (repl string)
  (let* ((output-stream (output-stream repl))
         (*standard-output* output-stream)
         (*error-output* output-stream)
         ;;(*debug-io* (make-two-way-stream *debug-io* output-stream))
         (*query-io* (make-two-way-stream *query-io* output-stream)))
    (with-graphic-debugger (repl)
      (with-eval-restarts
        (multiple-value-list
         (eval (read-from-string string)))))))

(defun %compile-string (repl string)
  (let* ((output-stream (output-stream repl))
         (*standard-output* output-stream)
         (*error-output* output-stream)
         ;;(*debug-io* (make-two-way-stream *debug-io* output-stream))
         (*query-io* (make-two-way-stream *query-io* output-stream)))
    (with-graphic-debugger (repl)
      (with-eval-restarts
        (multiple-value-list
         (compile-form string))))))

(defun adjust-history (new-input)
  (unless (equal new-input (car *repl-history*))
    (let ((stripped (string-trim #(#\Space #\Newline #\Tab #\Return)
                                 new-input)))
      (setf *repl-history*
            (cons stripped
                  (remove stripped *repl-history* :test #'equal))))))

(defun add-string-to-repl (text repl)
  (add-text-to-repl repl (make-instance 'text-item :text text)))

(defun add-result-to-repl (value repl)
  (add-text-to-repl repl (make-instance 'result-presentation
                                        :value value
                                        :text (prin1-to-string value))))

(defun concatenate-output-queue (queue)
  (with-output-to-string (str)
    (loop until (queue-empty-p queue)
          do (write-string (pop-queue queue) str))))

(defun insert-output (repl)
  (with-slots (output output-stream) repl
    (unless (queue-empty-p (queue output-stream))
      (let* ((string (concatenate-output-queue (queue output-stream)))
             (document (#_document output))
             (height (#_height (#_size document)))
             (output (output repl))
             (cursor (cursor output)))
        (#_movePosition cursor (#_QTextCursor::End))
        (#_insertText cursor string)
        (cond ((used output)
               (adjust-items-after-output repl
                                          (- (#_height (#_size document))
                                             height)
                                          t))
              (t
               (add-text-to-repl repl output :position (place output))
               (setf (used output) t)))))))

(defun insert-results (repl)
  (insert-output repl)
  (let ((results (channel-result (eval-channel repl))))
    (if results
        (loop for result in results
              do
              (push (add-result-to-repl result repl)
                    (things-to-move repl)))
        (push (add-string-to-repl "; No values" repl)
              (things-to-move repl)))
    (update-input repl)))

(defun display-results-minibuffer (repl)
  (insert-output repl)
  (with-slots (eval-channel) repl
    (let ((results (channel-result eval-channel)))
      (display (if results
                   (format nil "~{~s~^, ~}" results)
                   "; No values")
               *minibuffer*))))

(defun start-debugger (repl)
  (funcall (pop-queue (debugger-queue repl))))

(defun perform-evaluation (string repl result-to-minibuffer)
  (unwind-protect
       (evaluate-string repl string)
    (setf (current-package repl) *package*)
    (if result-to-minibuffer
        (emit-signal repl "displayResultsMinibuffer()")
        (emit-signal repl "insertResults()"))))

(defun perform-compilation (string repl)
  (unwind-protect
       (%compile-string repl string)
    (setf (current-package repl) *package*)
    (emit-signal repl "displayResultsMinibuffer()")))

(defun eval-string (string repl &key result-to-minibuffer)
  (submit-to-channel (eval-channel repl)
                     (lambda ()
                       (perform-evaluation string repl result-to-minibuffer))))

(defun compile-string (string repl)
  (submit-to-channel (eval-channel repl)
                     (lambda ()
                       (perform-compilation string repl))))

(defun evaluate (repl)
  (with-slots (input package-indicator item-position
               output-stream output
               eval-channel current-package
               things-to-move) repl
    (let ((string-to-eval (#_toPlainText input)))
      (#_setVisible input nil)
      (#_setVisible package-indicator nil)
      (setf things-to-move nil)
      (add-string-to-repl
       (format nil "~a> ~a"
               (short-package-name current-package) string-to-eval)
       repl)
      (when (used output)
        (setf output (make-instance 'repl-output)))
      (setf (place output) item-position)
      (adjust-history string-to-eval)
      (setf (history-index input) -1)
      (eval-string string-to-eval repl))))

(defun choose-history (window previous-p)
  (with-slots (input scene last-output-position) window
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
      (setf (history-index input) next-index)
      (#_setPlainText input (if (= next-index -1)
                                (current-input input)
                                (nth next-index *repl-history*)))
      (let ((cursor (#_textCursor input)))
        (#_movePosition cursor (#_QTextCursor::End))
        (#_setTextCursor input cursor)))))

(defun display-repl-arglist (window)
  (with-slots (input) window
    (let* ((parse (parse-lisp-string (#_toPlainText input))))
      (when parse
       (display-arglist parse (#_position (#_textCursor input))
                        *minibuffer*)))))
