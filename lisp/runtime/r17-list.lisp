;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 17 Sequences - List
;;; runtime/r17-list.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r17-list.lisp#2 $
;;;
;
(in-package #:si)

;;; [S]
;;; Native implementation of merge-sort. This function causes stack overflow
;;; for long list.
(defun stable-sort/list (list predicate key)
  (labels (
    (merge (left right)
      (with-collector (collect result)
        (loop
          (unless (and left right) (return))
           (if (funcall predicate (funcall key (first left))
                          (funcall key (first right)) )
               (collect (pop left))
             (collect (pop right)) ))
        (loop
          (when (endp left) (return))
          (collect (pop left)) )
        (loop
          (when (endp right) (return))
          (collect (pop right)) )
        (result) ) )

    (merge-sort (m)
      (if (null (cdr m))
          m
        (let ((left '())
              (right '()) )
          (loop
            (when (endp m) (return))
            (push (pop m) left)
            (when (endp m) (return))
            (push (pop m) right) )
          (merge (merge-sort left) (merge-sort right)) )) )
    )
    (merge-sort list) ) )
