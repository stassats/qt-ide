;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defstruct p-stream
  (string nil :type simple-string)
  (position 0 :type fixnum))

(defvar *p-base* 10)
(defvar *p-float-format* 'single-float)
(defvar *p-package* nil)
(defvar *p-stream*)
(defvar *no-p-wrappers* nil)
(defvar *end-of-file* (gensym "EOF"))

(defun p-read-char (p-stream)
  (let ((string (p-stream-string p-stream))
        (position (p-stream-position p-stream)))
    (cond ((end-of-p-stream-p p-stream)
           *end-of-file*)
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
           (let ((non-whitespace (position-if-not #'whitespace-char-p
                                                  string :start position)))
             (cond (non-whitespace
                    (setf (p-stream-position p-stream) non-whitespace)
                    (char string non-whitespace))
                   (t
                    *end-of-file*))))
          ((end-of-p-stream-p p-stream)
           *end-of-file*)
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
  (let ((start (1- (p-stream-position stream))))
    (loop for char = (p-read-char stream)
          for digit = (if (eq char *end-of-file*)
                          (return *end-of-file*)
                          (digit-char-p char))
          for number = digit then (if digit
                                      (+ (* number 10) digit)
                                      number)
          while digit
          finally
          (let* ((code (char-code (char-downcase char)))
                 (function (and (array-in-bounds-p table code)
                                (svref table code))))
            (if function
                (return (funcall function start number stream))
                (make-p-illegal :start start
                                :error (format nil "No dispatch macro for ~c." char)))))))

(defun invoke-reader-macro (reader-macro stream)
  (if (functionp reader-macro)
      (funcall reader-macro (1- (p-stream-position stream)) stream)
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

(defstruct (p
            (:constructor nil))
  (start nil :type unsigned-byte)
  (end (p-stream-position *p-stream*) :type unsigned-byte)
  error)

(defstruct (p-symbol
            (:include p))
  (name nil :type simple-string)
  (package nil :type (or package string null))
  (external nil :type boolean))

(defstruct (p-comment
            (:include p))
  (text nil :type simple-string))

(defstruct (p-function
            (:include p))
  name)

(defstruct (p-read-eval
            (:include p))
  form)

(defstruct (p-pathname
            (:include p))
  namestring)

(defstruct (p-number
            (:include p)
            (:constructor %make-p-number))
  (value nil :type number))

(defun make-p-number (&key start (end (p-stream-position *p-stream*))
                           value)
  (if *no-p-wrappers*
      value
      (%make-p-number :start start :end end :value value)))

(defstruct (p-list
            (:include p)
            (:constructor %make-p-list))
  (items nil :type list))

(defun make-p-list (&key start (end (p-stream-position *p-stream*))
                         error items)
  (if *no-p-wrappers*
      items
      (%make-p-list :start start :end end :items items
                    :error error)))

(defstruct (p-quote
            (:include p))
  object)

(defstruct (p-string
            (:include p)
            (:constructor %make-p-string))
  (value nil :type string))

(defun make-p-string (&key start (end (p-stream-position *p-stream*))
                           error value)
  (if *no-p-wrappers*
      value
      (%make-p-string :start start :end end :value value
                      :error error)))

(defstruct (p-vector
            (:include p))
  (value nil :type vector))

(defstruct (p-illegal
            (:include p)))

(defstruct (p-conditional
            (:include p))
  condition
  code)

(defun resolve-p-symbol (p-symbol)
  (if (symbolp p-symbol)
      p-symbol
      (let ((package (find-package (p-symbol-package p-symbol))))
        (and package
             (multiple-value-bind (symbol status) (find-symbol (p-symbol-name p-symbol) package)
               (and status
                    symbol))))))

(defun p-nth (n p-list)
  (nth n (p-list-items p-list)))

(defun resolve-form-operator (p-list)
  (and (p-list-p p-list)
       (p-symbol-p (p-nth 0 p-list))
       (resolve-p-symbol (car (p-list-items p-list)))))

(defun eval-conditional (cond)
  (labels ((eval-cond (cond)
             (typecase cond
               (cons
                (case (resolve-p-symbol (car cond))
                  (:not
                   (and (consp (cdr cond))
                        (not (eval-cond (cadr cond)))))
                  (:and
                   (and (listp (cdr cond))
                        (loop for (x . rest) on (cdr cond)
                              always (eval-cond x)
                              while (listp rest))))
                  (:or
                   (and (consp (cdr cond))
                        (loop for (x . rest) on (cdr cond)
                              thereis (and (eval-cond x) t)
                              while (listp rest))))))
               (p-symbol
                (and (member (resolve-p-symbol cond) *features*)
                     t)))))
    (eval-cond (p-conditional-condition cond))))

;;;

(defun parse-lisp-string (string)
  (let* ((*p-base* *read-base*)
         (*p-float-format* *read-default-float-format*)
         (*p-package* *package*)
         (stream (make-p-stream :string string))
         (*p-stream* stream))
    (parse-lisp-code stream)))

(defun parse-lisp-code (stream)
  (loop for code = (parse-lisp-form stream)
        until (eq code *end-of-file*)
        if (consp code)
        nconc code
        else
        collect code))

(defun parse-lisp-form (stream)
  (let* ((char (p-peek-char t stream))
         (rm-parser (when (characterp char)
                      (get-reader-macro-parser char nil))))
    (cond ((eq char *end-of-file*)
           *end-of-file*)
          (rm-parser
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

(defun parse-token (stream &optional symbol-p)
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
             (stop-p ()
               (or (end-p)
                   (terminating-char-p (p-peek-char nil stream))))
             (change-base (number)
               (if (= *p-base* 10)
                   number
                   (loop with result = 0
                         for multiplier = 1 then (* multiplier 10)
                         with remainder
                         do
                         (setf (values number remainder)
                               (truncate number *p-base*))
                         (incf result (* remainder multiplier))
                         when (zerop number)
                         return result)))
             (read-exponent ()
               (let ((multiple 1))
                 (when (stop-p)
                   (read-symbol))
                 (case (next-char)
                   (#\- (setf multiple -1))
                   (#\+)
                   (t
                    (unread)
                    (unless (digit-char-p char)
                      (read-symbol))))
                 (when (stop-p)
                   (read-symbol))
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
               (cond ((stop-p)
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
               (when (stop-p)
                 (read-symbol))
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
                                    (terminating-char-p
                                     (p-peek-char nil stream)))
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
                                          *p-package*)
                              symbol-name (if keyword
                                              (subseq string 1 end)
                                              string)))
                       (t
                        (setf symbol-name (subseq string (1+ colons)))
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
                                :external external
                                :start start)))
             (convert-case (char)
               (case (readtable-case *readtable*)
                 (:upcase (char-upcase char))
                 (:downcase (char-downcase char))
                 (:preserve char)
                 (:invert (if (upper-case-p char)
                              (char-downcase char)
                              (char-upcase char)))))
             (read-symbol ()
               (return-from parse-token
                 (create-symbol
                  (with-output-to-string (str)
                    (loop for i from start below (p-stream-position stream)
                          do (write-char (convert-case (char (p-stream-string stream) i))
                                         str))
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
                                 (write-char (convert-case char) str)))))))))
      (if symbol-p
          (read-symbol)
          (let ((number
                  (case (next-char)
                    (#\- (read-negative-number))
                    (#\+ (read-number))
                    (#\. (read-float))
                    (t
                     (unread)
                     (if (digit-p)
                         (read-number)
                         (read-symbol))))))
            ;; Only numbers are left, read-symbols does
            ;; return-from parse-token
            (make-p-number :start start
                           :value number))))))

;;;

(define-reader-macro-parser (#\)) (start stream)
  (declare (ignore stream))
  (make-p-illegal :start start
                  :error "Unmatched )"))

(define-reader-macro-parser (#\() (start stream)
  (let* (dot
         error
         (result
           (loop for char = (p-peek-char t stream)
                 when (eq char *end-of-file*)
                 do (setf error *end-of-file*)
                    (loop-finish)
                 until (char= char #\))
                 when dot
                 do (setf error "Dot in the middle.")
                 and collect dot
                 if (char= char #\.)
                 do
                 (p-read-char stream)
                 (setf dot (parse-lisp-form stream))
                 else
                 collect (parse-lisp-form stream)
                 finally (p-read-char stream))))
    (make-p-list :start start
                 :items (append result dot)
                 :error error)))

(define-reader-macro-parser (#\') (start stream)
  (make-p-quote :start start
                :object (parse-lisp-form stream)))

(define-reader-macro-parser (#\;) (start stream)
  (let ((end (loop when (end-of-p-stream-p stream)
                   return (p-stream-position stream)
                   when (char= (p-read-char stream) #\Newline)
                   return (1- (p-stream-position stream)))))
    (make-p-comment :start start
                    :end end ;;; FIXEM: should the newline be included or not
                    :text (subseq (p-stream-string stream) start end))))

(define-reader-macro-parser (#\") (start stream)
  (let (error)
    (make-p-string
     :start start
     :value
     (with-output-to-string (string)
       (loop with escaping
             for char = (p-read-char stream)
             do
             (cond ((eq char *end-of-file*)
                    (setf error *end-of-file*)
                    (return))
                   (escaping
                    (write-char char string)
                    (setf escaping nil))
                   ((char= char #\\)
                    (setf escaping t))
                   ((char= char #\")
                    (return))
                   (t
                    (write-char char string)))))
     :error error)))
;;;

(define-dispatching-char-parser (#\# :terminating nil))

(define-dispatching-macro-parser (#\# #\') (start parameter stream)
  (declare (ignore parameter))
  (let ((*no-p-wrappers* t))
    (make-p-function :start start
                     :name (parse-lisp-form stream))))

;;;

(defun parse-number-in-base (start *p-base* stream)
  (let ((number (parse-lisp-form stream)))
    (setf (p-start number) start)
    number))

(define-dispatching-macro-parser (#\# #\b) (start parameter stream)
  (declare (ignore parameter))
  (parse-number-in-base start 2 stream))


(define-dispatching-macro-parser (#\# #\o) (start parameter stream)
  (declare (ignore parameter))
  (parse-number-in-base start 8 stream))

(define-dispatching-macro-parser (#\# #\x) (start parameter stream)
  (declare (ignore parameter))
  (parse-number-in-base start 16 stream))

(define-dispatching-macro-parser (#\# #\r) (start base stream)
  (parse-number-in-base start base stream))

;;;

(define-dispatching-macro-parser (#\# #\c) (start parameter stream)
  (declare (ignore parameter))
  (let* ((cons (let ((*no-p-wrappers* t))
                 (parse-lisp-form stream))))
    (if (and (consp cons)
             (realp (car cons))
             (realp (cadr cons)))
        (make-p-number :start start
                       :value (complex (car cons) (cadr cons)))
        (make-p-illegal :start start
                        :error (format nil "bad complex: ~a" cons)))))

(define-dispatching-macro-parser (#\# #\() (start length stream)
  (p-unread-char stream)
  (let* ((list (p-list-items (parse-lisp-form stream)))
         (list-length (length list))
         (vector (make-array (or length list-length))))
    (replace vector list)
    (when length
      (fill vector (svref vector (1- list-length)) :start list-length))
    (make-p-vector :start start
                   :value vector)))

(define-dispatching-macro-parser (#\# #\.) (start parameter stream)
  (declare (ignore parameter))
  (make-p-read-eval :start start
                    :form (parse-lisp-form stream)))

(define-dispatching-macro-parser (#\# #\*) (start length stream)
  (flet ((bit-length ()
           (let* ((start (p-stream-position stream))
                  (string (p-stream-string stream))
                  (end (position-if (lambda (x) (char/= x #\1 #\0))
                                    string
                                    :start start)))
             (if (or (not end)
                     (terminating-char-p (aref string end)))
                 (- (or end (length string))
                    start)
                 (error "Bad #*")))))
    (let* ((bit-length (bit-length))
           (vector (make-array (or length bit-length) :element-type 'bit)))
      (loop for i below bit-length
            do (setf (sbit vector i) (digit-char-p (p-read-char stream))))
      (when length
        (fill vector (sbit vector (1- bit-length)) :start bit-length))
      (make-p-vector :start start
                     :value vector))))

(define-dispatching-macro-parser (#\# #\:) (start parameter stream)
  (declare (ignore parameter))
  (let* (*p-package*
         (symbol (parse-token stream t)))
    (setf (p-start symbol) start)
    symbol))

(define-dispatching-macro-parser (#\# #\p) (start parameter stream)
  (declare (ignore parameter))
  (let ((*no-p-wrappers* t))
   (make-p-pathname :start start
                    :namestring (parse-lisp-form stream))))

(defun parse-conditional (start stream &optional negate)
  (let ((condition (let ((*p-package* (find-package 'keyword))
                         (*no-p-wrappers* t))
                     (parse-lisp-form stream)))
        (end (p-stream-position *p-stream*)))
    (loop for code = (parse-lisp-form stream)
          until (eq code *end-of-file*)
          if (p-conditional-p code)
          collect code into after
          else
          collect code into protected
          until (and (not (p-conditional-p code))
                     (not (p-comment-p code)))
          finally
          (let ((cond (make-p-conditional :start start
                                          :end end
                                          :condition (if negate
                                                         `(:not ,condition)
                                                         condition)
                                          :code protected)))
            (return
              (if after
                  (cons cond after)
                  cond))))))

(define-dispatching-macro-parser (#\# #\+) (start parameter stream)
  (declare (ignore parameter))
  (parse-conditional start stream))

(define-dispatching-macro-parser (#\# #\-) (start parameter stream)
  (declare (ignore parameter))
  (parse-conditional start stream t))
