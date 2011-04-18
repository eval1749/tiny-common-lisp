;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 07 Objects
;;; runtime/r07-object.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r07-object.lisp#2 $
;;;
;
(in-package #:si)

(defun cl:ensure-generic-function (fname &rest initargs)
    (declare (values generic-function))
    (declare (type function-name fname))
    (declare (dynamic-extent initargs))
  (apply #'clos:ensure-generic-function-using-class
    (and (fboundp fname) (fdefinition fname))
    fname
    initargs ) )

(defun clos:extract-lambda-list (slambda-list)
  (si::with-collector (collect result)
    (let ((runner slambda-list))
      (loop
        (when (endp runner) (return))
        (let ((spec (first runner)))
          (cond
            ((symbolp spec)
              (when (member spec lambda-list-keywords) (return))
              (collect spec) )
            ((and (consp spec)
                  (consp (cdr spec))
                  (null (cddr spec)) )
              (collect (first spec)) )
            (t
              (error "Invalid specialized-lambda-list: ~S" slambda-list) ))
          (setq runner (cdr runner)) ))
        (nconc (result) runner) ) ) )

(defun clos:extract-specializer-names (slambda-list)
  (si::with-collector (collect result)
    (let ((runner slambda-list))
      (loop
        (when (endp runner) (return))
        (let ((spec (first runner)))
          (cond
            ((symbolp spec)
              (when (member spec lambda-list-keywords) (return))
              (collect t) )
            ((and (consp spec)
                  (consp (cdr spec))
                  (null (cddr spec)) )
              (collect (second spec)) )
            (t
              (error "Invalid specialized-lambda-list: ~S" slambda-list) ))
          (setq runner (cdr runner)) ))
      (result) ) ) )
