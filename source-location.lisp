(in-package #:qt-ide)

(defstruct source-location
  (path nil :type (or null pathname))
  (position nil :type (or null unsigned-byte)))

;;; SBCL

(defvar *source-types*
  '(:class
    :compiler-macro
    :condition
    :constant
    :function
    :generic-function
    :macro
    :method
    :method-combination
    :package
    :setf-expander
    :structure
    :symbol-macro
    :type
    :variable
    :optimizer
    :source-transform
    :transform
    :vop))

(defun convert-source-location (definition-source)
  (make-source-location
   :path
   (sb-introspect:definition-source-pathname definition-source)
   :position
   (sb-introspect:definition-source-character-offset definition-source)))

(defun find-source (p-symbol)
  (let ((symbol (resolve-p-symbol p-symbol)))
    (when symbol
      (loop for type in *source-types*
            for sources = (sb-introspect:find-definition-sources-by-name symbol type)
            when (and sources
                      (mapcar #'convert-source-location sources))
            nconc it))))
