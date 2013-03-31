;;; -*- Mode: Lisp -*-

(defpackage #:qt-ide
  (:use #:cl #:qt #:qt-ui
        #:trivial-gray-streams
        #:lparallel.queue)
  (:shadow window)
  (:export
   #:ide
   #:repl))
