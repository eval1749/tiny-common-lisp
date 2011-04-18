;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Macro - 07 Objects
;;; macro/m07-object.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m07-object.lisp#2 $
;;;
;
(in-package #:c)

(defmacro cl:defgeneric (fname lambda-list &rest options)
    (declare (ignore options))
 `(si::%defgeneric ',fname ',lambda-list) )

;;; <summary>
;;;   Simple implementation defmethod. This marco doesn't support:
;;;   <list>
;;;     <item><term>call-next-method</item>
;;;   </list>
;;; </summary>
(defmacro cl:defmethod (fname &rest runner)
  (labels (
    (collect-qualifiers ()
      (si::with-collector (collect qualifiers)
        (loop
          (let ((frob (first runner)))
            (when (consp frob) (return (qualifiers)))
            (collect (pop runner)) )) ) )

    (make-method-lambda (slambda-list specializer* body)
      (multiple-value-bind (decl* form*)
          (analyze-body body nil)
      (multiple-value-bind (lambda-list ldecl* lbind* bdecl*)
          (parse-lambda-list slambda-list)
     `(lambda ,lambda-list
            (declare (function-name (:method ,fname ,specializer*)))
            (declare ,@ldecl*)
            ,@decl*
        (let ,lbind* ,@bdecl*
           (block ,(if (consp fname) (second fname) fname)
             ,@form* ) ) ) ) ) )

    (make-specializer-form (spec)
      (cond
        ((symbolp spec)
          `(find-class ',spec))
        ((and (consp spec)
              (consp (cdr spec))
              (null  (cddr spec)) )
          `(clos:intern-eql-specializer ',(second spec)) )
        (t
          (error "Invalid specializer ~S" spec) )) )

    (parse-lambda-list (slambda-list)
      (si::with-collector (add-req req*)
      (si::with-collector (add-decl decl*)
      (si::with-collector (add-bind bind*)
      (si::with-collector (add-bind-decl bind-decl*)
        (let ((runner slambda-list))
          (loop
            (when (endp runner) (return))
            (let ((frob (first runner)))
              (when (member frob lambda-list-keywords) (return))
              (setq runner (cdr runner))
              (if (consp frob)
                  (let* ((var  (first frob))
                         (spec (second frob))
                         (req  (make-symbol (symbol-name var))) )
                    (add-req req)
                    (add-decl `(type ,spec ,req))
                    (add-bind `(,var ,req))
                    (add-bind-decl `(declare (ignorable ,var))) )
                (add-req frob) ) ))
          (values (nconc (req*) runner) (decl*) (bind*) (bind-decl*)) ))))) )
    )
  (let* ((qualifier*   (collect-qualifiers))
         (slambda-list (pop runner))
         (lambda-list  (clos:extract-lambda-list slambda-list))
         (specializer* (clos:extract-specializer-names slambda-list))
         (specializer-form* (mapcar #'make-specializer-form specializer*))
         (method-lambda
            (make-method-lambda slambda-list specializer* runner ))
         (initargs '()) )
   `(si::%defmethod
      ',fname
      ',qualifier*
      ',lambda-list
      (list ,@specializer-form*)
      ,method-lambda
      ,@initargs ) ) ) )
