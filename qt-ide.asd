;;; -*- Mode: Lisp -*-

(defsystem qt-ide
  :serial t
  :depends-on (qt qt-ui
                  alexandria
                  trivial-gray-streams
                  lparallel)
  :components ((:file "packages")
               (:file "channel")
               (:file "share")
               (:file "code-muncher")
               (:file "arglists")
               (:file "colorize")
               (:file "minibuffer")
               (:file "editor")
               (:file "qt-ide")
               (:file "debugger")
               (:file "streams")
               (:file "repl")
               (:file "inspector")))
