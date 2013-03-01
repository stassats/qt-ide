;;; -*- Mode: Lisp -*-

(defpackage #:qt-ide
  (:use #:cl #:qt #:qt-ui
        #:trivial-gray-streams)
  (:shadow window)
  (:export
   #:ide
   #:repl))
