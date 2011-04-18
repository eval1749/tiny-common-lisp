;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 17 Sequences - Vector
;;; runtime/r17-vector.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r17-vector.lisp#1 $
;;;
;
(in-package #:si)

;;; [S]

(defun stable-sort/vector (vector predicate key)
  (labels (
    (merge (vector temp left mid right)
      (let ((left-end (1- mid))
            (tmp-pos  left)
            (nelts    (1+ (- right left))) )
        (loop
          (unless (and (<= left left-end) (<= mid right))
            (return) )
          (if (funcall predicate (funcall key (elt vector left))
                                 (funcall key (elt vector mid)) )
              (progn
                (setf (elt temp tmp-pos) (elt vector left))
                (incf left) )
            (progn
              (setf (elt temp tmp-pos) (elt vector mid))
              (incf mid) ))
          (incf tmp-pos) )
        (loop
          (unless (<= left left-end) (return))
          (setf (elt temp tmp-pos) (elt vector left))
          (incf tmp-pos)
          (incf left) )
        (loop
          (unless (<= mid right) (return))
          (setf (elt temp tmp-pos) (elt vector mid))
          (incf tmp-pos)
          (incf mid) )
        (dotimes (i nelts)
          (setf (elt vector right) (elt temp right))
          (decf right) ) ) )
          
    (msort (vector temp left right)
      (when (> right left)
        (let ((mid (truncate (+ left right) 2)))
          (msort vector temp left mid)
          (msort vector temp (1+ mid) right)
          (merge vector temp left (1+ mid) right) )) )
    )
    (let ((temp (make-vector (length vector))))
      (msort vector temp 0 (1- (length vector)))
      vector ) ) )
