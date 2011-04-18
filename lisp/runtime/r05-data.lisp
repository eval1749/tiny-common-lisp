;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 05 Data and Control Flow
;;; runtime/r05-data.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r05-data.lisp#6 $
;;;
;
(in-package #:si)

;;; [E]
(defun cl:equal (x y)
  (or
    (eq x y)
    (typecase x
      (cons
        (and (consp y)
             (equal (car x) (car y))
             (equal (cdr x) (cdr y)) ) )
      (bit-vector
        (error "NYI: equal bit-vector") )
      (string
        (and (stringp y) (string= x y)) )
      (pathname
        (and (pathnamep y) (pathname-equal x y)) )
      (otherwise
        (eql x y) ))) )

(defun cl:equalp (x y)
  (typecase x
    (character
      (and (characterp y) (char-equal x y)) )
    (number
      (and (numberp y) (= x y)) )
    (cons
      (loop
        (unless (consp y) (return nil))
        (unless (equalp (car x) (car y)) (return nil))
        (setq x (cdr x))
        (setq y (cdr y))
        (unless (consp x) (return (equalp x y))) ) )
    (vector
      (and (vectorp x)
           (eql (length x) (length y))
           (dotimes (i (length x) t)
             (unless (equalp (elt x i) (elt y i)) (return nil)) )) )
    (array
      (and (arrayp y)
           (equal (array-dimensions x) (array-dimensions y))
           (eql (array-total-size x) (array-total-size y))
           (dotimes (i (array-total-size x) t)
             (unless (equalp (row-major-aref x i) (row-major-aref y i))
               (return nil) ) )) )
    (structure-object
      (error "NYI equalp structure") )
    (hash-table
      (error "NYI equalp hash-table") )
    (otherwise
      (equal x y) )) )

(defun cl:fdefinition (fname)
    (declare (values function))
    (declare (type function-name))
  (or (if (symbolp fname)
          (slot-value fname 'function)
        (let ((cell (find-setf-cell (second fname))))
          (and cell (slot-value cell 'function)) ))
      (error 'undefined-function :name fname) ) )

(defun (setf cl:fdefinition) (newfn fname)
    (declare (values function))
    (declare (type function-name fname))
    (declare (type function newfn))
  (if (symbolp fname)
      (let ((cell fname))
        (update-callers cell newfn)
        (setf (slot-value cell 'function) newfn) )
    (let ((cell (intern-setf-cell (second fname))))
      (update-callers cell newfn)
      (setf (slot-value cell 'function) newfn) ) ) )

(defun cl:fmakunbound (fname)
    (declare (values function-name))
    (declare (type function-name fname))
  (if (symbolp fname)
      (let ((cell fname))
        (update-callers cell (make-undefined-function cell))
        (setf (slot-value cell 'function) nil) )
    (let ((cell (find-setf-cell (second fname))))
      (when cell
        (update-callers cell (make-undefined-function cell))
        (setf (slot-value cell 'function) nil) ) ))
  fname )
