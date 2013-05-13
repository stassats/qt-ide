;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defclass inspector (navigable-viewer)
  ()
  (:metaclass qt-class)
  (:default-initargs
   :page-class (find-class 'inspector-page)))

(defclass inspector-page (viewer-page)
  ()
  (:metaclass qt-class))

(defclass inspector-link (qt-ui:link)
  ((parent-inspector :initarg :parent-inspector
                     :initform nil
                     :accessor parent-inspector))
  (:metaclass qt-class))

(defun inspector (object)
  (exec-window (make-instance 'inspector :object object)))

(defmethod view-object ((page inspector-page) object layout)
  (inspect-object object layout))

(defmethod view-link ((link link) object)
  (set-current-object (parent-inspector link) object))

;;;

(defgeneric inspect-object (object layout))
(defgeneric inspector-short-description (object))

;;;

(defmethod inspector-short-description :around (object)
  (let ((*print-circle* t))
    (call-next-method)))

(defmethod inspector-short-description (object)
  (prin1-to-string object))

;;;

(defmethod inspect-object :before (object layout)
  (output-text layout
               (with-output-to-string (stream)
                 (print-unreadable-object (object stream :type t :identity t)))
               :size 18))

(defmethod inspect-object (object layout)
  (output-text layout
               (with-output-to-string (stream)
                 (describe object stream)))
  (#_addStretch layout))

(defun add-row (layout description value &key link)
  (with-layout (box "QHBoxLayout" layout)
    (output-text box description :bold t)
    (#_addSpacing box 5)
    (if link
        (add-widgets box (make-instance 'inspector-link
                                        :object value
                                        :text (inspector-short-description value)
                                        :parent-inspector (#_parent (#_parent layout))))
        (output-text box value))
    (#_addStretch box)))

(defun format-iso8601-time (time-value &optional include-timezone-p)
  "Formats a universal time TIME-VALUE in ISO 8601 format, with
    the time zone included if INCLUDE-TIMEZONE-P is non-NIL"
  ;; Taken from http://www.pvv.ntnu.no/~nsaa/ISO8601.html
  ;; Thanks, Nikolai Sandved and Thomas Russ!
  (flet ((format-iso8601-timezone (zone)
           (if (zerop zone)
               "Z"
               (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                 ;; Tricky.  Sign of time zone is reversed in ISO 8601
                 ;; relative to Common Lisp convention!
                 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                         (> zone 0) h (round (* 60 m)))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
        (decode-universal-time time-value)
      (declare (ignore dow))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              include-timezone-p (format-iso8601-timezone (if dst
                                                              (+ zone 1)
                                                              zone))))))

(defmethod inspect-object ((integer integer) layout)
  (add-row layout
           "Value:"
           (format nil "~a = #x~x = #b~b = #o~o~@[ = ~e~]"
                   integer integer integer integer
                   (ignore-errors (coerce integer 'double-float))))
  (add-row layout
           "Integer-length:"
           (integer-length integer)
           :link t)
  (when (< -1 integer char-code-limit)
    (add-row layout
             "Code-char:"
             (code-char integer)
             :link t))
  (handler-case (format-iso8601-time integer t)
    (error ())
    (:no-error (time)
      (add-row layout
               "Universal-time:"
               time
               :link t)))
  (#_addStretch layout))

(defmethod inspect-object ((character character) layout)
  (add-row layout
           "Char-code:"
           (char-code character)
           :link t)
  (add-row layout
           "Char-name:"
           (char-name character)
           :link t)
  (add-row layout
           "Upper-case:"
           (char-upcase character)
           :link t)
  (add-row layout
           "Lower-case:"
           (char-downcase character)
           :link t)
  (#_addStretch layout))

(defmethod inspect-object ((class class) layout)
  (add-row layout
           "Name:"
           (class-name class)
           :link t)
  (add-row layout
           "Superclasses:"
           (c2mop:class-direct-superclasses class)
           :link t)
  (#_addStretch layout))
