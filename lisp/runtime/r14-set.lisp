;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 14 Conses - Set
;;; runtime/r14-set.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r14-set.lisp#2 $
;;;
;
(in-package #:si)

;; [A]
(defun cl:adjoin (item-1 list-2 &rest keyval*)
    (declare (values proper-list))
    (declare (type proper-list list-2))
  (if (apply #'member item-1 list-2 keyval*)
      list-2
    (cons item-1 list-2) ) )

;; [E]
(defun ensure-test-function (test test-not)
    (declare (values function-designator))
  (cond
    ((and test test-not)
      (error "Can't specify both :test and :test-not") )
    (test)
    (test-not (lambda (x y) (not (funcall test-not x y))) )
    (t #'eql) ) )

;; [M]
(defun cl:member (item list &key (key #'identity) test test-not)
    (declare (values proper-list))
    (declare (type proper-list list))
  (let ((test (ensure-test-function test test-not)))
    (let ((runner list))
      (loop
        (when (endp runner) (return nil))
        (when (funcall test (funcall key (car runner)) item)
          (return runner) )
        (setq runner (cdr runner)) ) ) ) )

(defun cl:member-if (pred alist &key (key #'identity))
    (declare (values proper-list))
    (declare (type function-designator pred))
    (declare (type proper-list alist))
  (let ((test (ensure-test-function test test-not)))
    (let ((runner list))
      (loop
        (when (endp runner) (return nil))
        (when (funcall pred (funcall key (car runner)))
          (return runner) )
        (setq runner (cdr runner)) ) ) ) )

