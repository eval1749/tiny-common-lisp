;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Macro - 09 Conditions
;;; macro/m09-cons.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m09-cond.lisp#4 $
;;;
;
(in-package #:c)

(defmacro cl:handler-bind ((&rest binding*) &rest form*)
  (if binding*
     `(%handler-bind
        ,@(mapcar (lambda (binding) (cadr binding)) binding*)
        (lambda () ,@form*) )
   `(progn ,@form*) ) )

(defmacro cl:ignore-errors (&rest form*)
  (let ((var '#:value))
   `(handler-case (progn ,@form*)
        (error (cond) (let ((,var nil)) (values ,var cond))) ) ) )

(defmacro cl:restart-bind (binding* &body body)
  `(restart-bind nil ,binding* ,@body) )

(defmacro restart-bind* (cond binding* &body body)
  (labels (
    (compute-restart (binding)
      (destructuring-bind (name function &rest keyval*) binding
        `(make-instance 'restart
            :function   ,function
            :name       ',name
            ,@keyval* ) ) )
    )
   `(with-restarts ,cond (list ,@(mapcar #'compute-restart binding*))
      ,@body ) ) )

(defmacro cl:restart-case (restartable-form &rest clause*)
  (let ((block '#:block)
        (cond  '#:cond)
        (var   '#:args) )
  (labels (
    (compute-condition (class arg*)
      `(si::coerce-to-condition ',class ,@arg*) )

    (parse-restartable-form (form)
      (let ((expansion (macroexpand form)))
        (case (first expansion)
          ((cerror) (parse-cerror expansion))
          ((error)  (parse-error  expansion 'simple-error))
          ((signal) (parse-signal expansion 'simple-condition))
          ((warn)   (parse-signal expansion 'simple-warning))
          (otherwise (values expansion nil)) ) ) )

    (parse-cerror (form)
      (values `(cerror ,(second form) ,cond)
              (compute-condition 'simple-error (cdddr form)) ) )

    (parse-clauses (clause*)
      (si::with-collector (collect-binding binding*)
      (si::with-collector (collect-handler handler*)
      (dolist (clause clause* (values (binding*) (handler*)))
        (destructuring-bind (case-name lambda-list &rest body) clause
          (si::with-collector (collect-keyval keyval*)
            (loop
              (case (first body)
                ((:interactive)
                  (pop body)
                  (collect-keyval :interactive-fucntion)
                  (collect-keyval `(function ,(pop body))) )
                ((:report)
                  (pop body)
                  (collect-keyval :report-function)
                  (let ((frob (pop body)))
                    (if (stringp frob)
                        (collect-keyval `(lambda (s) (write-string ,frob s)))
                      (collect-keyval `(function ,frob)) ) ) )
                ((:test)
                  (pop body)
                  (collect-keyval :test-fucntion)
                  (collect-keyval `(function ,(pop body))) )
                (otherwise
                  (return) )))
            (let ((tag (gensym)))
              (collect-binding
                `(,(first clause)
                    (lambda (&rest args) (setq ,var args) (go ,tag))
                    ,@(keyval*) ))
              (collect-handler tag)
              (collect-handler
                `(return-from ,block
                    (apply (lambda ,lambda-list ,@body) ,var) )) )))))) )

    (parse-error (form class)
      (values `(,(first form) ,cond) 
              (compute-condition class (cddr form)) ) )

    (parse-signal (form class)
      (multiple-value-bind (form cond-form)
          (parse-error form class)
        (values `(return-from ,block ,form) cond-form) ) )
    )
    (multiple-value-bind (rs-form cond-form)
        (parse-restartable-form restartable-form)
    (multiple-value-bind (binding* handler*)
        (parse-clauses clause*)
     `(block ,block
       (let (,var)
         (tagbody
           (let ((,cond ,cond-form))
             (restart-bind* ,cond ,binding* ,rs-form) )
           ,@handler* ) )) ) ) ) ) )

(defmacro cl:with-condition-restarts (condition restarts &body body)
 `(with-restarts condition restarts ,@body) )

;;; Note: this macro is used by restart-case, user program may not
;;; use this macro explicitly.
;;;
;;;= <FIXME date="2009-01-12" by="yosi@msn.com">
;;;=    We should use si:make-restart-cluster function to validate
;;;=    condition and restart list.
;;;= </FIXME>
(defmacro with-restarts (condition restarts &body body)
 `(let ((si::*restart-clusters*
          (cons (cons ,condition ,restarts) si::*restart-clusters*) ))
    (declare (dynamic-extent si::*restart-clusters*))
    ,@body ) )

(defmacro cl:with-simple-restart ((name format-control &rest format-arg*)
                                    &rest form* )
 `(restart-case (progn ,@form*)
    (,name ()
      :report
        (lambda (stream)
            (declare (function-name (with-simple-restart :report)))
          (format stream ,format-control ,@format-arg*) )
      (values nil t) ) ) )
