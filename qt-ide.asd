;;; -*- Mode: Lisp -*-

(defsystem qt-ide
  :serial t
  :depends-on (qt qt-ui
                  alexandria
                  trivial-gray-streams)
  :components ((:file "packages")
               (:file "share")
               (:file "qt-ide")
               (:file "debugger")
               (:file "streams")
               (:file "repl")
               (:file "inspector")))
