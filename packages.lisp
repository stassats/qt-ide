;;; -*- Mode: Lisp -*-

(defpackage #:qt-ide
  (:use #:cl #:qt #:qt-ui)
  (:shadow window)
  (:export
   #:ide
   #:repl))
