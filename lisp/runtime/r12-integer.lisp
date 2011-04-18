;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 12 Numbers - Integer
;;; runtime/r12-integer.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r12-integer.lisp#2 $
;;;
;
(in-package #:si)

;;; [L]

(defun cl:logbitp (index integer)
    (declare (values t))
    (declare (type unsigned-byte index))
    (declare (type integer integer))
  (ldb-test (byte 1 index) integer) )

(defun cl:lognot (x)
    (declare (values integer))
    (declare (type integer x))
  (logxor x -1) )
