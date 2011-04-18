;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 17 Sequences
;;; runtime/r17-sequence.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r17-sequence.lisp#2 $
;;;
;
(in-package #:si)

;;; [B]
(defun bounding-index-error (seq start end)
    (declare (type sequence seq))
  (error 'bounding-index-error
    :end        end
    :sequence   seq
    :start      start ) )

;;; [S]
(defun sequence-index-error (seq index)
    (declare (type sequence seq))
  (error 'unbound-index
    :datum    index
    :expected-type `(integer 0 ,(1- (length seq)))
    :sequence seq ) )

(defun cl:stable-sort (sequence predicate &key (key 'identity))
  (typecase sequence
    (list   (stable-sort/list   sequence predicate key))
    (vector (stable-sort/vector sequence predicate key))
    (otherwise
      (error 'type-error :datum sequence :expected-type 'sequence) )) )

(defun cl:sort (sequence predicate &rest args)
  (apply 'stable-sort sequence predicate args) )
