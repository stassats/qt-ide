(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *editor-table* (make-key-table))

(define-command-prefix "C-c" *editor-table*)

(defclass editor-tabs ()
  ((editors :initarg :editors
            :initform nil
            :accessor editors))
  (:metaclass qt-class)
  (:qt-superclass "QTabWidget"))

(defmethod initialize-instance :after ((widget editor-tabs) &key parent)
  (new-instance widget parent)
  (let ((editor (make-instance 'editor :parent widget)))
    (#_setDocumentMode widget t)
    (#_setTabsClosable widget t)
    (#_addTab widget editor "scratch")))

(defun open-file (file editor-tab)
  (let* ((file (probe-file file))
         (contents (and file (alexandria:read-file-into-string file)))
         (editor (and file (make-instance 'editor :parent editor-tab
                                                  :file file))))
    (when file
      (#_setPlainText (text-edit editor) contents)
      (#_addTab editor-tab editor (file-namestring file))
      (#_setCurrentWidget editor-tab editor)
      (push editor (editors editor-tab))
      editor)))

;;;

(defclass editor ()
  ((file :initarg :file
         :initform nil
         :accessor file)
   (text-edit :initform nil
              :accessor text-edit)
   (status-bar :initform nil
               :accessor status-bar))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((editor editor) &key parent
                                                            file)
  (new-instance editor parent)
  (let* ((vbox (#_new QVBoxLayout))
         (status-bar (#_new QStatusBar))
         (package-label (#_new QLabel))
         (text-edit (make-instance 'text-edit
                                   :parent editor
                                   :editor-tabs parent
                                   :status-bar status-bar
                                   :file file
                                   :package-label package-label)))
    (setf (text-edit editor) text-edit)
    (#_setLayout editor vbox)
    (#_setContentsMargins vbox 0 0 0 0)
    (add-widgets vbox text-edit status-bar)
    (#_addWidget status-bar package-label)))

(defclass text-edit ()
  ((file :initarg :file
         :initform nil
         :accessor file)
   (modified :initform nil
             :accessor modified)
   (parsed :initform nil
           :accessor parsed)
   (package-map :initarg :package-map
                :initform nil
                :accessor package-map)
   (timer :initform nil
          :accessor timer)
   (flashed-region :initform nil
                   :accessor flashed-region)
   (status-bar :initarg :status-bar
               :initform nil
               :accessor status-bar)
   (current-package :initform (find-package :cl-user)
                    :accessor current-package)
   (package-label :initarg :package-label
                  :initform nil
                  :accessor package-label)
   (editor-tabs :initarg :editor-tabs
                :initform nil
                :accessor editor-tabs))
  (:metaclass qt-class)
  (:qt-superclass "QTextEdit")
  (:slots ("changed()" text-changed)
          ("cursorChanged()" cursor-changed)
          ("removeFlash()" remove-flash))
  (:override ("keyPressEvent" key-press-event)))

(defmethod initialize-instance :after ((editor text-edit) &key parent)
  (new-instance editor parent)
  (let ((timer (#_new QTimer editor)))
    (setf (timer editor) timer)
    (#_setSingleShot timer t)
    (#_setFont editor *default-qfont*)
    (#_setAcceptRichText editor nil)
    ;;(#_setCursorWidth editor 0)
    (connect editor "textChanged()"
             editor "changed()")
    (connect editor "cursorPositionChanged()"
             editor "cursorChanged()")
    (connect timer "timeout()" editor "removeFlash()")))

(defun text-changed (editor)
  (with-signals-blocked (editor)
    (let* ((code (parse-lisp-string (#_toPlainText editor)))
           (cursor (#_textCursor editor))
           (position (#_position cursor)))
      (colorize code cursor)
      (display-arglist code position *minibuffer*)
      (setf (parsed editor) code
            (package-map editor) (make-package-map code)))))

(defun cursor-changed (editor)
  ;;(:dbg (#_blockNumber (#_textCursor editor)))
  (let* ((position (#_position (#_textCursor editor)))
         (package-name (package-at-position position (package-map editor)))
         (package (and package-name
                       (find-package package-name))))
    (setf (current-package editor) (or package (find-package :cl-user)))
    (#_setText (package-label editor)
               (format nil "Package: ~a" (or package-name "n/a")))
    (display-arglist (parsed editor) position *minibuffer*)))

(defun top-level-form (editor)
  (let ((form (find-top-level-form (#_position (#_textCursor editor))
                                   (parsed editor)))
        (text (#_toPlainText editor)))
    (when form
      (values (subseq text (p-start form)
                      (p-end form))
              form))))

(defun visit-file (file tabs)
  (let ((file (probe-file file)))
    (if file
        (loop for editor in (editors tabs)
              when (equal file (file editor))
              return editor
              finally (return (open-file file tabs)))
        (display (format nil "File ~s not found" file) *minibuffer*))))

(defun go-to (editor position)
  (#_moveCursor editor (#_QTextCursor::End))
  (let ((cursor (#_textCursor editor)))
    (#_setPosition cursor position)
    (#_setTextCursor editor cursor)))

(defun visit-source-location (editor location)
  (let ((file (source-location-path location))
        (position (source-location-position location))
        (tabs (editor-tabs editor)))
    (if file
        (let ((editor (visit-file file tabs)))
          (when (and editor position)
            (go-to (text-edit editor) (+ position 2))
            (#_setCurrentWidget tabs editor)))
        (display "No file in source location." *minibuffer*))))

;;;

(defmethod key-press-event ((editor text-edit) event)
  (multiple-value-bind (command description)
      (get-key-command (#_key event) (#_modifiers event) *editor-table*)
    (case command
      (:insert (stop-overriding))
      (:undefined (display (format nil "~a is undefined" description)
                           *minibuffer*))
      ((nil)
       (display description *minibuffer*))
      (t
       (display "" *minibuffer*)
       (when command
         (funcall command editor))))))

(defun remove-flash (editor)
  (with-signals-blocked (editor)
      (clear-background (shiftf (flashed-region editor) nil)
                        (#_textCursor editor))))

(defun flash-region (p-form editor)
  (with-signals-blocked (editor)
    (colorize-background-form p-form (#_textCursor editor)
                              *flash-color*)
    (setf (flashed-region editor) p-form)
    (#_start (timer editor) 200)))

(define-command "C-c C-c" 'compile-tlf *editor-table*)
(defun compile-tlf (editor)
  (multiple-value-bind (form p-form)
      (top-level-form editor)
    (cond (form
           (flash-region p-form editor)
           (eval-string form *repl* :result-to-minibuffer t))
          ((display "No form around cursor" *minibuffer*)))))

(defun move-cursor (editor where)
  (let ((cursor (#_textCursor editor)))
    (#_movePosition cursor where)
    (#_setTextCursor editor cursor)))

(define-command "C-a" 'move-start-of-line *editor-table*)
(defun move-start-of-line (editor)
  (move-cursor editor (#_QTextCursor::StartOfBlock)))

(define-command "C-e" 'move-end-of-line *editor-table*)
(defun move-end-of-line (editor)
  (move-cursor editor (#_QTextCursor::EndOfBlock)))

(define-command "C-p" 'move-previous-line *editor-table*)
(defun move-previous-line (editor)
  (move-cursor editor (#_QTextCursor::Up)))

(define-command "C-n" 'move-next-line *editor-table*)
(defun move-next-line (editor)
  (move-cursor editor (#_QTextCursor::Down)))

(define-command "C-f" 'move-next-char *editor-table*)
(defun move-next-char (editor)
  (move-cursor editor (#_QTextCursor::NextCharacter)))

(define-command "C-b" 'move-previous-char *editor-table*)
(defun move-previous-char (editor)
  (move-cursor editor (#_QTextCursor::PreviousCharacter)))

(define-command "<tab>" 'complete *editor-table*)
(defun complete (editor)
  (let* ((rect (#_cursorRect editor))
         (x (#_x rect))
         (y (+ (#_y rect) (#_height rect)))
         (position (#_position (#_textCursor editor)))
         (package (current-package editor))
         (symbol (find-symbol-at-position position (parsed editor)))
         (list (and symbol
                    (make-instance 'completer
                                   :package package
                                   :query symbol
                                   :parent editor))))
    (when list
      (#_move list x y)
      (#_show list)
      (#_setFocus list))))

(define-command "M-." 'edit-source *editor-table*)
(defun edit-source (editor)
  (let* ((position (#_position (#_textCursor editor)))
         (symbol (find-symbol-at-position position (parsed editor)))
         (definitions (find-source symbol)))
    (if definitions
        (visit-source-location editor (car definitions))
        (display (format nil "No definition of ~a."
                         (print-p-symbol symbol (current-package editor)))
                 *minibuffer*))))
