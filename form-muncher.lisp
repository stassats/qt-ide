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

(defun operator-form-p (operator form)
  (let ((p-symbol (and (p-list-p form)
                       (car (p-list-items form)))))
    (when (and (p-symbol-p p-symbol)
               (eq (resolve-p-symbol p-symbol) operator))
      (values (cdr (p-list-items form))
              t))))

(defun symbol-designator-name (p-symbol-designator)
  (typecase p-symbol-designator
    (p-symbol (p-symbol-name p-symbol-designator))
    (p-string  (p-string-value p-symbol-designator))
    (t "n/a")))

(defun make-package-map (forms)
  (loop for form in forms
        when
        (multiple-value-bind (args found) (operator-form-p 'in-package form)
          (when found
            (cons form
                  (symbol-designator-name (car args)))))
        collect it))

(defun package-at-position (position map)
  (let ((result "n/a"))
    (loop for (in-package-form . package-name) in map
          for start = (p-start in-package-form) 
          when (>= position start)
          do (setf result package-name)
          until (> start position))
    result))
