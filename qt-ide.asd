;;; -*- Mode: Lisp -*-

(defsystem qt-ide
  :serial t
  :depends-on (qt qt-ui
                  alexandria)
  :components ((:file "packages")
               (:file "qt-ide")
               (:file "debugger")
               (:file "repl")))
