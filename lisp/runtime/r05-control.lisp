;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 05 Data and Control Flow
;;; runtime/r05-control.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r05-control.lisp#1 $
;;;
;
(in-package #:si)

(defun cl:apply (fn param &rest param*)
    (declare (dynamic-extent param*))
  (if (null param*)
      (multiple-value-call fn (values-list param))
    (let* ((args (list param))
           (tail args)
           (runner param*) )
      (loop
        (let ((arg (pop runner)))
          (when (endp runner)
            (setf (cdr tail) arg)
            (return (multiple-value-call fn (values-list args))) )
          (setq tail (setf (cdr tail) (list arg))) )) )) )
