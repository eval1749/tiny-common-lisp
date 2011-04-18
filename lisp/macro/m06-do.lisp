;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Macro - 06 Iteration
;;; macro/m06-do.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m06-do.lisp#1 $
;;;
;
(in-package #:c)

(labels (
  (expand-do (binding* test-form result-form* body op-let op-setq)
    (si::with-collector (collect-bind let-bind*)
    (si::with-collector (collect-decl let-decl*)
    (si::with-collector (collect-step step-pair*)
    (dolist (binding binding*)
      (cond
        ((symbolp binding)
          (collect-bind binding) )
        ((consp binding)
          (let* ((runner binding)
                 (var      (pop runner))
                 (initform (pop runner))
                 (stepformp runner)
                 (stepform  (pop runner)) )
              (when runner
                (error "Invalid do-binding: ~S" binding) )
            (collect-bind (list var initform))
            (if stepformp
                (progn
                  (collect-step var)
                  (collect-step stepform) )
              (collect-decl `(declare (ignorable ,var))) ) ) )
        (t
          (error "Malformed do-binding: ~S" binding) )) )
    (multiple-value-bind (decl* form*) (analyze-body body nil)
     `(block nil
        (,op-let ,(let-bind*) ,@(let-decl*) ,@decl*
          (tagbody
            (go .test)
           .loop
            ,@form*
             (,op-setq ,@(step-pair*))
           .test
           ,(if test-form
                `(if (not ,test-form) (go .loop))
               `(go .loop) ))
           ,@result-form* )) ) ))) )
    )
    (defmacro cl:do (binding* (test-form &rest result-form*) &body body)
      (expand-do binding* test-form result-form* body 'let 'psetq) )

    (defmacro cl:do* (binding* (test-form &rest result-form*) &body body)
      (expand-do binding* test-form result-form* body 'let* 'setq) )
  ) ; labels

