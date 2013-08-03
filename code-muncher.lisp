;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defstruct p-stream
  (string nil :type simple-string)
  (position 0 :type fixnum))

(defun p-read-char (p-stream)
  (let ((string (p-stream-string p-stream))
        (position (p-stream-position p-stream)))
    (cond ((end-of-p-stream-p p-stream)
           (error 'end-of-file))
          (t
           (prog1 (char string position)
             (setf (p-stream-position p-stream) (1+ position)))))))

(defun whitespace-char-p (char)
  (find char #(#\Space #\Tab #\Newline #\Return)))

(defun end-of-p-stream-p (p-stream)
  (= (p-stream-position p-stream) (length (p-stream-string p-stream))))

(defun p-peek-char (&optional peek-type p-stream)
  (let ((string (p-stream-string p-stream))
        (position (p-stream-position p-stream)))
    (cond (peek-type
           (let ((non-whitespace (position-if-not #'whitespace-char-p string :start position)))
             (unless non-whitespace
               (error 'end-of-file))
             (setf (p-stream-position p-stream) non-whitespace)
             (char string non-whitespace)))
          ((end-of-p-stream-p p-stream)
           (error 'end-of-file))
          (t
           (char string position)))))

(defun p-unread-char (p-stream)
  (when (minusp (decf (p-stream-position p-stream)))
    (error "Beginning of stream ~a" p-stream)))
;;;

(defvar *reader-macros-parsers*
  (make-array 255 :initial-element nil))

(defvar *non-terminating-reader-macros-parsers*
  (make-array 255 :initial-element nil))

(defmacro define-reader-macro-parser ((char &key (terminating t)) lambda-list &body body)
  (let ((name (intern (format nil "~a-~a" 'reader-macro-parser char))))
    `(progn (defun ,name ,lambda-list
              ,@body)
            (setf (svref ,(if terminating
                              '*reader-macros-parsers*
                              '*non-terminating-reader-macros-parsers*)
                         ,(char-code char))
                  #',name)
            ',name)))

(defun get-reader-macro-parser (char &optional (terminating t))
  (let ((code (char-code char))
        (array (if terminating
                   *reader-macros-parsers*
                   *non-terminating-reader-macros-parsers*)))
    (and (array-in-bounds-p array code)
         (svref array code))))

(defun terminating-char-p (char)
  (or (whitespace-char-p char)
      (get-reader-macro-parser char)))

;;;

(defstruct p-symbol
  (name nil :type simple-string)
  (package nil :type (or package string null))
  (external nil :type boolean))

(defstruct p-comment
  (text nil :type simple-string))

;;; 

(defun parse-lisp-string (string)
  (parse-lisp-code (make-p-stream :string string)))

(defun parse-token (stream)
  (let ((start (p-stream-position stream))
        (number 0)
        char)
    (labels ((next-char ()
               (setf char (p-read-char stream)))
             (unread ()
               (p-unread-char stream))
             (end-p ()
               (end-of-p-stream-p stream))
             (read-float ()
               (+ number
                  (setf number 0)
                  (loop with decimal = 1
                        until (end-p)
                        do
                        (next-char)
                        (cond ((terminating-char-p char)
                               (unread)
                               (loop-finish))
                              ((digit-char-p char)
                               (setf decimal (* decimal 10)
                                     number (+ (* number 10) (digit-char-p char))))
                              (t
                               (unread)
                               (read-symbol)))
                        
                        finally
                        (when (= decimal 1)
                          (error "Dot context error."))
                        (return (/ number (float decimal))))))
             (read-ratio ()
               (cond ((end-p)
                      (unread)
                      (read-symbol))
                     (t
                      (/ number
                         (loop initially (setf number 0)
                               do
                               (next-char)
                               (cond ((terminating-char-p char)
                                      (unread)
                                      (return number))
                                     ((digit-char-p char)
                                      (setf number (+ (* number 10) (digit-char-p char))))
                                     (t
                                      (unread)
                                      (read-symbol)))
                               (when (end-p)
                                 (return number)))))))
             (read-number ()
               (loop (next-char)
                     (cond ((terminating-char-p char)
                            (unread)
                            (return number))
                           ((digit-char-p char)
                            (setf number (+ (* number 10) (digit-char-p char))))
                           ((char= char #\.)
                            (return (read-float)))
                           ((char= char #\/)
                            (return (read-ratio)))
                           (t
                            (unread)
                            (read-symbol)))
                     (when (end-p)
                       (return number))))
             (read-negative-number ()
               (- (read-number)))
             (create-symbol (end)
               (let* ((string (p-stream-string stream))
                      (keyword (char= (char string start) #\:))
                      (colons (position #\: string :start (1+ start) :end end
                                                   :from-end t))
                      external
                      package
                      symbol-name)
                 (when (and keyword colons)
                   (error "Colons and keywords"))
                 (when (eql colons end)
                   (error "Colons at the end"))
                 (cond ((not colons)
                        (setf package (if keyword
                                          (find-package :keyword)
                                          *package*)
                              symbol-name (if keyword
                                              (subseq string (1+ start) end)
                                              (subseq string start end))))
                       (t
                        (setf symbol-name (subseq string colons end))
                        (cond
                          ((char= (char string (1- colons)) #\:)
                           (decf colons))
                          (t
                           (setf external t)))
                        (when (find #\: string :start start :end (1- colons))
                          (error "Too many colons"))
                        (setf package (subseq string 0 colons))))
                 (make-p-symbol :name symbol-name
                                :package package
                                :external external)))
             (read-symbol ()
               (loop (next-char)
                     (cond ((terminating-char-p char)
                            (unread)
                            (return-from parse-token
                              (create-symbol (p-stream-position stream))))
                           ((end-p)
                            (return-from parse-token
                              (create-symbol (p-stream-position stream)))))))
             (read-token ()
               (case (next-char)
                 (#\- (read-negative-number))
                 (#\+ (read-number))
                 (#\. (read-float))
                 (t
                  (unread)
                  (if (digit-char-p char)
                      (read-number)
                      (read-symbol))))))
      (read-token))))

(defun parse-lisp-code (stream)
  (let* ((char (p-peek-char t stream))
         (rm-parser (get-reader-macro-parser char)))
    (cond (rm-parser
           (p-read-char stream)
           (funcall rm-parser stream))
          (t
           (parse-token stream)))))

;;;

(define-reader-macro-parser (#\)) (stream)
  (declare (ignore stream))
  (error ") encountered"))

(define-reader-macro-parser (#\() (stream)
  (loop with dot
        for char = (p-peek-char t stream)
        until (char= char #\))
        if (char= char #\.)
        do
        (p-read-char stream)
        (setf dot (parse-lisp-code stream))
        else
        collect (parse-lisp-code stream) into result
        and when dot do (error "Dot in the middle.")
        finally (return (append result dot))))

(define-reader-macro-parser (#\') (stream)
  (list 'quote (parse-lisp-code stream)))

(define-reader-macro-parser (#\;) (stream)
  (let ((start (p-stream-position stream)))
    (flet ((make-comment (end)
             (make-p-comment :text (subseq (p-stream-string stream) start end))))
      (loop when (end-of-p-stream-p stream)
            return (make-comment (p-stream-position stream))
            when (char= (p-read-char stream) #\Newline)
            return (make-comment (1- (p-stream-position stream)))))))
