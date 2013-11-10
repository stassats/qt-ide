(defsystem qt-ide
  :serial t
  :depends-on (qt qt-ui
                  alexandria
                  trivial-gray-streams
                  lparallel
                  #+sbcl sb-introspect)
  :components ((:file "packages")
               (:file "channel")
               (:file "share")
               (:file "code-muncher")
               (:file "form-muncher")
               (:file "arglists")
               (:file "colorize")
               (:file "key-bindings")
               (:file "compilation")
               (:file "completion")
               (:file "minibuffer")
               (:file "source-location")
               (:file "editor")
               (:file "qt-ide")
               (:file "debugger")
               (:file "streams")
               (:file "repl")
               (:file "inspector")))
