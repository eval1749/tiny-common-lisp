;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 12 Numbers - Number
;;; runtime/r12-number.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r12-number.lisp#2 $
;;;
;
(in-package #:si)

;;; [1]
(defun cl:1+ (x)
    (declare (values number))
    (declare (type number x))
  (+ x 1) )

(defun cl:1- (x)
    (declare (values number))
    (declare (type number x))
  (- x 1) )

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

;;; [Z]
(defun cl:zerop (x)
    (declare (values t))
    (declare (type number x))
  (= x 0) )
