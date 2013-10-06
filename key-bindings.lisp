;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)
(named-readtables:in-readtable :qt)

(defconstant +control-code+ #x1000021)
(defconstant +alt-code+ #x1000023)
(defconstant +shift-code+ #x1000020)
(defconstant +meta-code+ #x01000022)

(defconstant +control-modifier+ #x4000000)
(defconstant +alt-modifier+ #x8000000)
(defconstant +shift-modifier+ #x2000000)

(defun parse-key-binding (string)
  (let ((stream (make-p-stream :string string)))
    (labels ((parse-part ()
               (let ((alt 0)
                     (control 0))
                 (loop for char = (p-read-char stream)
                       for next-char = (p-peek-char nil stream)
                       until (or (eql next-char *end-of-file*)
                                 (eql next-char #\Space))
                       do
                       (case char
                         (#\C
                          (assert (zerop control))
                          (assert (eql (p-read-char stream)
                                       #\-))
                          (setf control +control-modifier+))
                         (#\M
                          (assert (zerop alt))
                          (assert (eql (p-read-char stream)
                                       #\-))
                          (setf alt +alt-modifier+)))
                       finally
                       (return (logior alt control
                                       (if (upper-case-p char)
                                           (logior +shift-modifier+
                                                   (char-code char))
                                           (char-code (char-upcase char)))))))))
      (loop while (not (eq (p-peek-char t stream) *end-of-file*))
            collect (parse-part)))))

(defun key-to-string (keys)
  (flet ((print-part (key)
           (declare ((unsigned-byte 32) key))
           (let ((char (code-char (ldb (byte 21 0) key)))
                 (control (logtest key +control-modifier+))
                 (alt (logtest key +alt-modifier+))
                 (shift (logtest key +shift-modifier+)))
             (format nil "~:[~;C-~]~:[~;M-~]~c" control alt
                     (if shift
                         char
                         (char-downcase char))))))
    (if (listp keys)
        (format nil "~{~a~^ ~}" (mapcar #'print-part keys))
        (print-part keys))))

(defstruct key-table
  (table (make-hash-table))
  state
  sub-table)

(defun find-prefix-table (path key-table)
  (if path
      (loop for (part . rest) on path
            for parent = (key-table-table key-table) then sub-table
            for sub-table = (gethash part parent)
            then (gethash part sub-table)
            do (cond ((null sub-table)
                      (if rest
                          (error "~s in ~s is not a prefix binding."
                                 (key-to-string part)
                                 (key-to-string path))
                          (return parent)))
                     ((null rest)
                      (return sub-table))
                     ((not (hash-table-p sub-table))
                      (error "~s in ~s is a binding for command ~a"
                             (key-to-string part)
                             (key-to-string path)
                             sub-table))))
      (key-table-table key-table)))

(defun define-command-prefix (key key-table)
  (let* ((parsed (parse-key-binding key))
         (key (car (last parsed)))
         (sub-table (find-prefix-table (nbutlast parsed) key-table)))
    (setf (gethash key sub-table) (make-hash-table))))

(defun define-command (key command key-table)
  (check-type command symbol)
  (let* ((parsed (parse-key-binding key))
         (last-part (car (last parsed)))
         (sub-table (find-prefix-table (nbutlast parsed) key-table)))
    (if (hash-table-p (gethash last-part sub-table))
        (error "~a already names a prefix" key)
        (setf (gethash last-part sub-table) command))))

(defun get-key-command (key modifiers key-table)
  (declare ((unsigned-byte 32) key modifiers))
  (cond ((or (zerop modifiers)
             (= modifiers +shift-modifier+)
             (= key +control-code+)
             (= key +alt-code+)
             (= key +shift-code+)
             (= key +meta-code+))
         (setf (key-table-sub-table key-table) nil
               (key-table-state key-table) nil)
         (values :insert nil))
        (t
         (let* ((key (logior key modifiers))
                (table (or (key-table-sub-table key-table)
                           (key-table-table key-table)))
                (command (gethash key table)))
           (etypecase command
             (null
              (setf (key-table-sub-table key-table) nil)
              (values :undefined
                      (key-to-string
                       (nreverse
                        (cons key
                              (shiftf (key-table-state key-table) nil))))))
             (hash-table
              (setf (key-table-sub-table key-table) command)
              (push key (key-table-state key-table))
              (values nil
                      (key-to-string
                       (reverse (key-table-state key-table)))))
             (symbol
              (setf (key-table-sub-table key-table) nil
                    (key-table-state key-table) nil)
              (values command nil)))))))