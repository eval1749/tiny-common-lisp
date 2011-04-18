;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Macros
;;; genesis/genesis-macro.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis-runtime.lisp#8 $
;;;
;
(in-package #:si)

(macrolet (
  (define+ (ty op* op/2 op/1)
   `(defun ,op* (x &rest y*)
        (declare (values real))
        (declare (type ,ty x))
        (declare (dynamic-extent y*))
      (if (null y*)
          (,op/1 x)
        (dolist (y y* x) (setq x (,op/2 x y))) )) )

  (define* (ty op* op/2 identity)
   `(defun ,op* (&rest y*)
        (declare (values ,ty))
        (declare (dynamic-extent y*))
      (let ((x ,identity))
        (dolist (y y* x)
          (setq x (,op/2 x y)) ) ) ) )
  )
  ;;
  (define* number cl:+ +/2   0)
  (define+ real   cl:- -/2 -/1)
  (define* number cl:* */2   1)
  (define+ real   cl:/ //2 //1)

  (define* integer cl:logand logand/2 -1)
  (define* integer cl:logeqv logeqv/2 -1)
  (define* integer cl:logior logior/2  0)
  (define* integer cl:logxor logxor/2  0)
 ) ; macrolet

(defun cl:1+ (n)
    (declare (values number))
    (declare (type number n))
  (+ n 1) )

(defun cl:1- (n)
    (declare (values number))
    (declare (type number n))
  (- n 1) )

(macrolet (
  (define-<> (ty op* op/2)
   `(defun ,op* (x &rest y*)
        (declare (values t))
        (declare (type ,ty x y*))
        (declare (dynamic-extent y*))
      (if (null y*)
          (the ,ty x)
        (dolist (y y* t)
          (unless (,op/2 x y) (return nil))
          (setq x y) )) ) )

  (define-== (ty op* op/2)
   `(defun ,op* (x &rest y*)
        (declare (values t))
        (declare (type ,ty x y*))
        (declare (dynamic-extent y*))
      (if (null y*)
          (the ,ty x)
        (dolist (y y* t)
          (unless (,op/2 x y) (return nil)) )) ) )

  (define-/= (ty op* op/2)
   `(defun ,op* (x &rest y*)
        (declare (values t))
        (declare (type ,ty x y*))
        (declare (dynamic-extent y*))
      (loop
        (when (null y*) (return t))
        (dolist (y y*)
          (unless (,op/2 x y) (return-from ,op* nil)) )
        (pop y*) ) ) )
  )
  ;;
  (define-<> real cl:<  </2)
  (define-<> real cl:<= <=/2)
  (define-<> real cl:>  >/2)
  (define-<> real cl:>= >=/2)

  (define-== number cl:=  =/2)
  (define-/= number cl:/= /=/2)

  (define-<> character cl:char<  char</2)
  (define-<> character cl:char<= char<=/2)
  (define-<> character cl:char>  char>/2)
  (define-<> character cl:char>= char>=/2)

  (define-== character cl:char=  char=/2)
  (define-/= character cl:char/= char/=/2)

  (define-<> character cl:char-lessp         char-lessp/2)
  (define-<> character cl:char-not-greaterp  char-not-greaterp/2)
  (define-<> character cl:char-greaterp      char-greaterp/2)
  (define-<> character cl:char-not-lessp     char-not-lessp/2)

  (define-== character cl:char-equal         char-equal/2)
  (define-/= character cl:char-not-equal     char-not-equal/2)
) ; macrolet

(defun mapcar/1 (fn list-1)
  (with-collector (collect)
    (dolist (item-1 list-1)
      (collect (funcall fn item-1)) ) ) )

(defun mapcar/2 (fn list-1 list-2)
  (with-collector (collect)
    (let ((runner-1 list-1)
          (runner-2 list-2) )
      (loop
        (when (or (endp runner-1) (endp runner-2))
          (return) )
        (collect (funcall fn (pop runner-1) (pop runner-2))) ) ) ) )
