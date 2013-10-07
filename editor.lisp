;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defvar *editor-table* (make-key-table))

(define-command-prefix "C-c" *editor-table*)

(defclass editor ()
  ((minibuffer :initarg :minibuffer
                    :initform nil
                    :accessor minibuffer)
   (parsed :initform nil
           :accessor parsed)
   (repl :initarg :repl
         :initform nil
         :accessor repl)
   (timer :initform nil
          :accessor timer)
   (flashed-region :initform nil
                   :accessor flashed-region))
  (:metaclass qt-class)
  (:qt-superclass "QTextEdit")
  (:slots ("changed()" text-changed)
          ("cursorChanged()" cursor-changed)
          ("removeFlash()" remove-flash))
  (:override ("keyPressEvent" key-press-event)))

(defmethod initialize-instance :after ((editor editor) &key parent)
  (new-instance editor parent)
  (let ((timer (#_new QTimer editor)))
    (setf (timer editor) timer)
    (#_setSingleShot timer t)
    (#_setFont editor *default-qfont*)
    (#_setAcceptRichText editor nil)
                                        ;(#_setCursorWidth editor 0)
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
      (display-arglist code position (minibuffer editor))
      (setf (parsed editor) code))))

(defun cursor-changed (editor)
  ;(:dbg (#_blockNumber (#_textCursor editor)))
  (display-arglist (parsed editor)
                   (#_position (#_textCursor editor))
                   (minibuffer editor)))

(defun top-level-form (editor)
  (let ((form (find-top-level-form (#_position (#_textCursor editor))
                                   (parsed editor)))
        (text (#_toPlainText editor)))
    (when form
      (values (subseq text (p-start form)
                      (p-end form))
              form))))

(defmethod key-press-event ((editor editor) event)
  (multiple-value-bind (command description)
      (get-key-command (#_key event) (#_modifiers event) *editor-table*)
    (case command
      (:insert (stop-overriding))
      (:undefined (display (format nil "~a is undefined" description)
                           (minibuffer editor)))
      ((nil)
       (display description (minibuffer editor)))
      (t
       (display "" (minibuffer editor))
       (when command
         (funcall command editor))))))

(defun remove-flash (editor)
  (clear-background (shiftf (flashed-region editor) nil)
                    (#_textCursor editor)))

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
           (eval-string form (repl editor) :result-to-minibuffer t))
          ((display "No form around cursor" (minibuffer editor))))))

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
