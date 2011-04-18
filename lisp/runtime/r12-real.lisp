;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 12 Numbers - Real
;;; runtime/r12-real.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r12-real.lisp#1 $
;;;
;
(in-package #:si)

;;; [M]

(defun cl:minusp (x)
    (declare (values t))
    (declare (type real x))
  (< x 0) )

;;; [P]

(defun cl:plusp (x)
    (declare (values t))
    (declare (type real x))
  (> x 0) )
