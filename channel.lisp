;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ide)

(defvar *end-channel* (gensym "END-CHANNEL"))

(defstruct (channel)
  (eval-queue (make-queue))
  (result-queue (make-queue)))

(defun call-in-channel (channel function)
  "Call a function in the channel, blocking for the result."
  (push-queue function (channel-eval-queue channel))
  (values-list (pop-queue (channel-result-queue channel))))

(defun submit-to-channel (channel function)
  "Submit a function to the channel without waiting for the result."
  (push-queue function (channel-eval-queue channel)))

(defun channel-result (channel)
  "Recieve result, blocking to wait for it."
  (values-list (pop-queue (channel-result-queue channel))))

(defun channel-loop (channel)
  (let ((eval-queue (channel-eval-queue channel))
        (result-queue (channel-result-queue channel)))
    (unwind-protect
         (loop with result
               with success
               for function = (pop-queue eval-queue)
               until (eq function *end-channel*)
               do
               (setf success nil)
               (block nil
                 (unwind-protect
                      (setf result
                            (multiple-value-list (funcall function))
                            success t)
                   (unless success
                     (push-queue nil result-queue))))
               (push-queue result result-queue))
      (setf (channel-eval-queue channel) nil
            (channel-result-queue channel) nil))))

(defun channel-alive-p (channel)
  (and (channel-eval-queue channel)
       (channel-result-queue channel)
       t))

(defun exit-channel-loop (channel)
  (submit-to-channel channel *end-channel*))

(defun make-channel-thread (&optional name)
  (let ((channel (make-channel)))
    (values channel
            (bt:make-thread (lambda ()
                              (channel-loop channel))
                            :name name))))
(defun flush-queue (queue)
  (with-locked-queue queue
    (loop until (queue-empty-p/no-lock queue)
          do (pop-queue/no-lock queue))))

(defun flush-channel (channel)
  (flush-queue (channel-eval-queue channel))
  (flush-queue (channel-result-queue channel)))
