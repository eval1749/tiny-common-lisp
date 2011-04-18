;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 04 Types and Classes
;;; runtime/r04-type.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r04-type.lisp#1 $
;;;
;
(in-package #:si)

(defun get-classd (name)
  (slot-value (find-class name) 'instance-description) )

(defun subclassp (class-1 class-2)
    (declare (type class class-1 class-2))
  (or
    (eq class-1 class-2)
    (let ((cpl-1 (slot-value class-1 'class-precedence-list)))
      (if cpl-1
          (memq class-2 cpl-1)
        (dolist (super-1 (slot-value class-1 'direct-superclasses))
          (when (subclassp super-1 class-2) (return t)) )) )) )
