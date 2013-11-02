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

(defun find-symbol-at-position (position forms)
  (labels ((inside (form)
             (< (p-start form)
                position
                (1+ (p-end form))))
           (at (form)
             (< (p-start form)
                position
                (1+ (p-end form))))
           (find-form (form)
             (cond ((p-conditional-p form)
                    (some #'find-form (p-conditional-code form)))
                   ((and (p-symbol-p form)
                         (at form))
                    form)
                   ((and (p-list-p form)
                         (inside form))
                    (loop for (x . rest) on (p-list-items form)
                          thereis (find-form x)
                          while (consp rest))))))
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

(defun make-package-map (forms)
  (loop for form in forms
        when
        (multiple-value-bind (args found) (operator-form-p 'in-package form)
          (when found
            (cons form
                  (or (and (consp args)
                           (symbol-designator-name (car args)))
                      "n/a"))))
        collect it))

(defun package-at-position (position map)
  (let (result)
    (loop for (in-package-form . package-name) in map
          for start = (p-start in-package-form) 
          when (>= position start)
          do (setf result package-name)
          until (> start position))
    result))
