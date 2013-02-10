;;; -*- Mode: Lisp -*-

(defsystem qt-ide
  :serial t
  :depends-on (qt qt-ui
                  alexandria)
  :components ((:file "packages")
               (:file "share")
               (:file "qt-ide")
               (:file "debugger")
               (:file "repl")
               (:file "inspector")))
