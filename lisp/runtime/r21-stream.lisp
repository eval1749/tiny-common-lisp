;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 21 Streams
;;; runtime/r21-stream.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r21-stream.lisp#3 $
;;;
;
(in-package #:si)

;;; [E]

;; Note: We don't use (declare (values input-stream)) here.
;; input-stream-p is true for stream classes not inherited from
;; input-stream.
(defun ensure-input-stream (s)
    (declare (values stream))
    #+nil (declare (type input-stream-designator s))
  (case s
    ((nil) *standard-input*)
    ((t) *terminal-io*)
    (otherwise
      (if (input-stream-p s)
          s
        (error 'type-error
            :datum s
            :expected-type 'input-stream-designator )) )) )

;; Note: We don't use (declare (values output-stream)) here.
;; output-stream-p is true for stream classes not inherited from
;; output-stream.
(defun ensure-output-stream (s)
    (declare (values stream))
    #+nil (declare (type output-stream-designator s))
  (case s
    ((nil) *standard-output*)
    ((t) *terminal-io*)
    (otherwise
      (if (output-stream-p s)
          s
        (error 'type-error
            :datum s
            :expected-type 'output-stream-designator )) )) )

;;; [F]
(defun cl:fresh-line (&optional d)
    (declare (values t))
    (declare (type output-stream-designator d))
  (let ((s (ensure-output-stream d)))
    (unless (stream-start-line-p s)
      (stream-write-char s #\Newline)) ) )

;;; [I]
(defun cl:input-stream-p (s)
    (declare (type stream s))
  (logbitp 0 (slot-value s 'flags)) )

;;; [O]
(defun cl:output-stream-p (s)
    (declare (type stream s))
  (logbitp 1 (slot-value s 'flags)) )

;;; [T]
(defun cl:terpri (&optional d)
    (declare (values null))
    (declare (type output-stream-designator d))
  (let ((s (ensure-output-stream d)))
    (stream-write-char s #\Newline)
    nil ) )
