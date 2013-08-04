;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defstruct p-stream
  (string nil :type simple-string)
  (position 0 :type fixnum))

(defvar *p-base* 10)
(defvar *p-float-format* 'single-float)

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

(defmacro define-reader-macro-parser ((char &key (terminating t)) lambda-list &body body)
  (let ((name (intern (format nil "~a-~a" 'reader-macro-parser char))))
    `(progn (defun ,name ,lambda-list
              ,@body)
            (setf (svref *reader-macros-parsers*
                         ,(char-code char))
                  (cons #',name ,terminating))
            ',name)))

(defun get-reader-macro-parser (char &optional (terminating t))
  (let* ((code (char-code char))
         (macro (and (array-in-bounds-p *reader-macros-parsers* code)
                     (svref *reader-macros-parsers* code))))
    (and macro
         (or (not terminating)
             (cdr macro))
         (car macro))))

(defun terminating-char-p (char)
  (or (whitespace-char-p char)
      (get-reader-macro-parser char)))

(defmacro define-dispatching-char-parser ((char &key (terminating t)))
  (let ((name (intern (format nil "~a-~a" 'reader-macro-parser char))))
    `(progn (setf (svref *reader-macros-parsers* ,(char-code char))
                  (cons (or (car (svref *reader-macros-parsers* ,(char-code char)))
                            (make-array 255 :initial-element nil))
                        ,terminating))
            ',name)))

(defun invoke-dispatching-reader-macro (table stream)
  (loop for char = (p-read-char stream)
        for digit = (digit-char-p char)
        for number = digit then (if digit
                                    (+ (* number 10) digit)
                                    number)
        while digit
        finally
        (let ((function (and (array-in-bounds-p table (char-code char))
                             (svref table (char-code char)))))
          (if function
              (return (funcall function number stream))
              (error "No dispatch macro for ~c." char)))))

(defun invoke-reader-macro (reader-macro stream)
  (if (functionp reader-macro)
      (funcall reader-macro stream)
      (invoke-dispatching-reader-macro reader-macro stream)))

(defmacro define-dispatching-macro-parser ((dispatch-char char) lambda-list &body body)
  (let ((name (intern (format nil "~a-~a~a" 'reader-macro-parser dispatch-char char))))
    `(progn (defun ,name ,lambda-list
              ,@body)
            (setf (svref (car (svref *reader-macros-parsers* ,(char-code dispatch-char)))
                         ,(char-code char))
                  #',name)
            ',name)))
;;;

(defstruct p-symbol
  (name nil :type simple-string)
  (package nil :type (or package string null))
  (external nil :type boolean))

(defstruct p-comment
  (text nil :type simple-string))

(defstruct p-function
  (name nil :type (or p-symbol symbol cons)))

;;; 

(defun parse-lisp-string (string)
  (parse-lisp-code (make-p-stream :string string)))

(defun parse-lisp-code (stream)
  (let* ((char (p-peek-char t stream))
         (rm-parser (get-reader-macro-parser char nil)))
    (cond (rm-parser
           (p-read-char stream)
           (invoke-reader-macro rm-parser stream))
          (t
           (parse-token stream)))))

(defun exponent-char-p (char)
  (find char "esfdl" :test #'char-equal))

(defun exponent-float-format (char)
  (or (cdr (assoc char '((#\s short-float)
                         (#\f single-float)
                         (#\d double-float)
                         (#\l long-float))
                  :test #'char-equal))
      *p-float-format*))

(defun parse-token (stream)
  (let ((start (p-stream-position stream))
        (number 0)
        char
        digit)
    (labels ((next-char ()
               (setf char (p-read-char stream)))
             (digit-p ()
               (setf digit (digit-char-p char *p-base*)))
             (unread ()
               (p-unread-char stream))
             (end-p ()
               (end-of-p-stream-p stream))
             (change-base (number)
               (if (= *p-base* 10)
                   number
                   (loop with result = 0
                         for multiplier = 1 then (* multiplier 10)
                         with remainder
                         do
                         (setf (values number remainder) (truncate number *p-base*))
                         (incf result (* remainder multiplier))
                         when (zerop number)
                         return result)))
             (read-exponent ()
               (let ((multiple 1))
                 (when (end-p)
                   (read-symbol))
                 (case (next-char)
                   (#\- (setf multiple -1))
                   (#\+)
                   (t
                    (unread)
                    (unless (digit-char-p char)
                      (read-symbol))))
                 (loop with result = 0
                       until (end-p)
                       do
                       (next-char)
                       (cond ((terminating-char-p char)
                              (unread)
                              (loop-finish))
                             ((digit-p)
                              (setf result (+ (* result 10) digit)))
                             (t
                              (unread)
                              (read-symbol)))
                        
                       finally
                       (return (* multiple result)))))
             (make-float-with-exponent (divisor exponent)
               (let ((format (exponent-float-format exponent))
                     (exponent (read-exponent)))
                 (coerce (/ (* (expt 10 exponent) number) divisor) format)))
             (read-float ()
               (setf number (change-base number))
               (loop with *p-base* = 10
                     with divisor = 1
                     until (end-p)
                     do
                     (next-char)
                     (cond ((terminating-char-p char)
                            (unread)
                            (loop-finish))
                           ((exponent-char-p char)
                            (return (make-float-with-exponent divisor char)))
                           ((digit-p)
                            (setf divisor (* divisor 10)
                                  number (+ (* number 10) digit)))
                           (t
                            (unread)
                            (read-symbol)))
                        
                     finally
                     (when (= divisor 1)
                       (error "Dot context error."))
                     (return (coerce (/ number divisor) *p-float-format*))))
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
                                     ((digit-p)
                                      (setf number (+ (* number *p-base*) digit)))
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
                           ((digit-p)
                            (setf number (+ (* number *p-base*) digit)))
                           ((exponent-char-p char)
                            (setf number (change-base number))
                            (return (make-float-with-exponent 1 char)))
                           ((char= char #\.)
                            (if (or (end-p)
                                    (terminating-char-p (p-peek-char nil stream)))
                                (return (change-base number))
                                (return (read-float))))
                           ((char= char #\/)
                            (return (read-ratio)))
                           (t
                            (unread)
                            (read-symbol)))
                     (when (end-p)
                       (return number))))
             (read-negative-number ()
               (- (read-number)))
             (create-symbol (string)
               (let* ((end (length string))
                      (keyword (char= (char string 0) #\:))
                      (colons (position #\: string :start 1 :end end :from-end t))
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
                                              (subseq string 1 end)
                                              string)))
                       (t
                        (setf symbol-name (subseq string colons))
                        (cond
                          ((char= (char string (1- colons)) #\:)
                           (decf colons))
                          (t
                           (setf external t)))
                        (when (find #\: string :end (1- colons))
                          (error "Too many colons"))
                        (setf package (subseq string 0 colons))))
                 (make-p-symbol :name symbol-name
                                :package package
                                :external external)))
             (read-symbol ()
               (return-from parse-token
                 (create-symbol
                  (with-output-to-string (str)
                    (write-string (p-stream-string stream) str
                                  :start start
                                  :end (p-stream-position stream))
                    (loop with escaping
                          with multi-escaping
                          until (end-p)
                          do
                          (next-char)
                          (cond (escaping
                                 (write-char char str)
                                 (setf escaping nil))
                                ((char= char #\|)
                                 (setf multi-escaping (not multi-escaping)))
                                ((char= char #\\)
                                 (setf escaping t))
                                (multi-escaping
                                 (write-char char str))
                                ((terminating-char-p char)
                                 (unread)
                                 (return))
                                (t
                                 (write-char char str)))))))))
      (case (next-char)
        (#\- (read-negative-number))
        (#\+ (read-number))
        (#\. (read-float))
        (t
         (unread)
         (if (digit-p)
             (read-number)
             (read-symbol)))))))

;;;

(define-reader-macro-parser (#\)) (stream)
  (declare (ignore stream))
  (error ") encountered"))

(define-reader-macro-parser (#\() (stream)
  (:dbg stream)
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
        finally
        (p-read-char stream)
        (return (append result dot))))

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

(define-reader-macro-parser (#\") (stream)
  (with-output-to-string (string)
    (loop with escaping
          for char = (p-read-char stream)
          do
          (cond (escaping
                 (write-char char string)
                 (setf escaping nil))
                ((char= char #\\)
                 (setf escaping t))
                ((char= char #\")
                 (return))
                (t
                 (write-char char string))))))
;;;

(define-dispatching-char-parser (#\# :terminating nil))

(define-dispatching-macro-parser (#\# #\') (parameter stream)
  (declare (ignore parameter))
  (make-p-function :name (parse-lisp-code stream)))

;;;

(define-dispatching-macro-parser (#\# #\b) (parameter stream)
  (declare (ignore parameter))
  (let ((*p-base* 2))
    (parse-lisp-code stream)))

(define-dispatching-macro-parser (#\# #\o) (parameter stream)
  (declare (ignore parameter))
  (let ((*p-base* 8))
    (parse-lisp-code stream)))

(define-dispatching-macro-parser (#\# #\x) (parameter stream)
  (declare (ignore parameter))
  (let ((*p-base* 16))
    (parse-lisp-code stream)))

(define-dispatching-macro-parser (#\# #\r) (*p-base* stream)
  (parse-lisp-code stream))

;;;
