;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defun find-form-around-position (position forms)
  (labels ((inside (form)
             (< (p-start form)
                position
                (if (eq (p-error form) *end-of-file*)
                    (1+ (p-end form))
                    (p-end form))))
           (find-form (form)
             (cond ((p-conditional-p form)
                    (some #'find-form (p-conditional-code form)))
                   ((not (inside form))
                    nil)
                   ((p-list-p form)
                    (or
                     (loop for (x . rest) on (p-list-items form)
                           if (and (p-list-p x)
                                   (not (null (p-list-items x)))
                                   (find-form x))
                           return it
                           while (consp rest))
                     form))
                   (t
                    form))))
    (some #'find-form forms)))

(defun find-top-level-form (position forms)
  (labels ((inside (form)
             (< (p-start form)
                position
                (if (eq (p-error form) *end-of-file*)
                    (1+ (p-end form))
                    (p-end form))))
           (find-form (form)
             (cond ((p-conditional-p form)
                    (some #'find-form (p-conditional-code form)))
                   ((inside form)
                    form))))
    (some #'find-form forms)))

(defun form-arglist (position forms)
  (let* ((form (find-form-around-position position forms))
         (list (and (p-list-p form)
                    (p-list-items form)))
         (symbol (and (consp list)
                      (typep (car list) 'p-symbol)
                      (resolve-p-symbol (car list)))))
    (when symbol
      (let ((*print-case* :downcase)
            (*print-pretty* nil)
            (arglist (get-arglist symbol)))
        (when arglist
          (with-output-to-string (*standard-output*)
            (print-arglist position arglist form)))))))

(defun get-raw-arglist (symbol)
  (cond ((fboundp symbol)
         (sb-introspect:function-lambda-list symbol))))

(defun get-arglist (symbol)
  (let ((arglist (get-raw-arglist symbol)))
    (when arglist
      (parse-arglist symbol arglist))))

(defstruct arglist
  operator
  required
  optional
  key
  rest
  body)

(defstruct optional-arg
  var
  supplied-p
  has-initform
  initform)

(defstruct (keyword-arg
            (:include optional-arg))
  keyword)

(defun parse-optional-arg (x &optional keyword-p)
  (let (var
        supplied-p
        has-initform
        initform
        keyword)
    (flet ((make-keyword (symbol)
             (when keyword-p
               (setf keyword
                     (make-p-symbol :start 0 :end 0
                                    :name (string symbol)
                                    :package (find-package 'keyword)
                                    :external t)))))
      (cond ((listp x)
             (destructuring-bind (&optional x-var
                                            (x-initform nil x-initform-p)
                                            (x-supplied nil x-supplied-p)) x
               (setf has-initform x-initform-p
                     initform x-initform)
               (setf supplied-p x-supplied)
               (cond ((not (listp x-var))
                      (setf var x-var)
                      (make-keyword var))
                     (keyword-p
                      (destructuring-bind (keyword-name x-var) x-var
                        (setf keyword keyword-name)
                        (setf var
                              (if (listp x-var)
                                  (parse-arglist nil x-var)
                                  x-var))))
                     (t
                      (setf var (parse-arglist nil x-var))))))
            (t
             (setf var x)
             (make-keyword x))))
    (if keyword-p
        (make-keyword-arg
         :var var
         :supplied-p supplied-p
         :has-initform has-initform
         :initform initform
         :keyword keyword)
        (make-optional-arg
         :var var
         :supplied-p supplied-p
         :has-initform has-initform
         :initform initform))))

(defun parse-arglist (operator arglist)
  (let (optional
        key
        rest
        body
        required)
    (labels ((find-parser (x)
               (getf (list '&environment #'&env
                           '&key #'&key
                           '&optional #'&optional
                           '&body #'&body
                           '&rest #'&rest
                           '&aux #'&aux)
                     x))
             (process-next ()
               (let ((parser (find-parser (pop arglist))))
                 (when parser
                   (funcall parser))))
             (destructure (x)
               (if (listp x)
                   (parse-arglist nil x)
                   x))
             (&env ()
               (pop arglist)
               (process-next))
             (&optional ()
               (setf optional
                     (loop while (and arglist
                                      (not (find-parser (car arglist))))
                           collect (parse-optional-arg (pop arglist))))
               (process-next))
             (&rest ()
               (setf rest (pop arglist))
               (process-next))
             (&body ()
               (setf body (pop arglist))
               (process-next))
             (&key ()
               (setf key
                     (loop while (and arglist
                                      (not (find-parser (car arglist))))
                           collect (parse-optional-arg (pop arglist) t)))
               (process-next))
             (&aux ()
               (loop while (and arglist
                                (not (find-parser (car arglist))))
                     do (pop arglist))
               (process-next))
             (required ()
               (setf required
                     (loop while (and arglist
                                      (not (find-parser (car arglist))))
                           collect (destructure (pop arglist))))
               (process-next)))
      (when (eq (car arglist) '&environment)
        (pop arglist)
        (pop arglist))
      (required)
      (make-arglist
       :operator operator
       :required required
       :optional optional
       :key key
       :rest rest
       :body body))))

(defun print-key-symbol (symbol)
  (if (p-symbol-p symbol)
      (string-downcase (p-symbol-name symbol))
      (string-downcase (symbol-name symbol))))

(defun print-keyword-arg (optional)
  (if (keyword-arg-has-initform optional)
      (format nil "(~a ~s)"
              (keyword-arg-var optional)
              (keyword-arg-initform optional))
      (format nil "~a"
              (keyword-arg-var optional))))

(defun print-optional-arg (optional)
  (if (optional-arg-has-initform optional)
      (format nil "(~a ~s)"
              (optional-arg-var optional)
              (optional-arg-initform optional))
      (format nil "~a"
              (optional-arg-var optional))))

(defun symbols-match-p (symbol p-symbol)
  (let* ((symbol-name (if (symbolp symbol)
                          (symbol-name symbol)
                          (p-symbol-name symbol)))
         (symbol-package (if (symbolp symbol)
                             (symbol-package symbol)
                             (p-symbol-package symbol)))
         (symbol-package-name (if (packagep symbol-package)
                                  (package-name symbol-package)
                                  symbol-package)))
    (and (equal symbol-name
                (p-symbol-name p-symbol))
         (equal symbol-package-name
                (if (packagep (p-symbol-package p-symbol))
                    (package-name (p-symbol-package p-symbol))
                    (p-symbol-package p-symbol))))))

(defun print-arglist (position arglist form)
  (write-char #\()
  (let ((list (and (p-list-p form)
                   (p-list-items form)))
        (highlighted nil)
        print-space)
    (labels ((print-space ()
               (if print-space
                   (write-char #\Space)
                   (setf print-space t)))
             (print-highlighted (x)
               (setf highlighted t)
               (format t "<span style=\"background-color:#b4eeb4\">~a</span>"
                       x))
             (inside (form)
               (typecase form
                 (p
                  (<= (p-start form) position (p-end form)))
                 (cons
                  (some #'inside form))
                 (null
                  (not highlighted))))
             (print-arg (form arg &optional (highlight t))
               (print-space)
               (if (and (inside form)
                        highlight)
                   (print-highlighted arg)
                   (princ arg)))
             (required ()
               (loop for arg in (arglist-required arglist)
                     for form = (pop list)
                     do (cond ((arglist-p arg)
                               (print-space)
                               (print-arglist position arg form))
                              (t
                               (print-arg form arg)))))
             (&rest ()
               (when (arglist-body arglist)
                 (print-space)
                 (princ "&body")
                 (print-arg list (arglist-body arglist)
                            (not (arglist-key arglist))))
               (when (arglist-rest arglist)
                 (print-space)
                 (princ "&rest")
                 (print-arg list (arglist-rest arglist)
                            (not (arglist-key arglist)))))
             (&optional ()
               (when (arglist-optional arglist)
                 (format t " &optional")
                 (loop for arg in (arglist-optional arglist)
                       for form = (pop list)
                       do
                       (print-arg form (print-optional-arg arg)))))
             (find-key (keyword)
               (and (not highlighted)
                    (let ((keyword-name (keyword-arg-keyword keyword)))
                      (loop for (key parameter) on list
                            when (symbols-match-p key keyword-name)
                            return (or (inside key)
                                       (inside parameter))))))
             (&key ()
               (when (arglist-key arglist)
                 (print-space)
                 (princ "&key")
                 (loop for arg in (arglist-key arglist)
                       for printed = (print-keyword-arg arg)
                       do
                       (print-space)
                       (if (find-key arg)
                           (print-highlighted printed)
                           (princ printed))))))
      (when (arglist-operator arglist)
        (princ (arglist-operator arglist))
        (print-space)
        ;; Prevent the next arg from being highlighted if there's
        ;; no space between the operator
        (when (inside (pop list))
          (setf highlighted t)))
      (required)
      (&optional)
      (&rest)
      (&key)))
  (write-char #\)))
