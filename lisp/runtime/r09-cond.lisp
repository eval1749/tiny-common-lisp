;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 09 Conditions
;;; runtime/r07-cond.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r09-cond.lisp#1 $
;;;
;
(in-package #:si)

(defun invoke-restart-internal (name condition errorp &rest args)
    (declare (dynamic-extent args))
  (let ((restart (find-restart name condition)))
    (cond
      (restart
        (apply (slot-value restart 'function) args) )
      (errorp
        (error 'restart-not-found
            :condition condition
            :name      name ) )) ) )

(macrolet (
  (define (name arg* errorp)
    `(defun ,name (,@arg* &optional condition)
       (invoke-restart-internal ',name condition ,errorp ,@arg*) ) )
  )
  (define cl:abort          nil t)
  (define cl:continue       nil nil)
  (define cl:muffle-warning nil t)
  (define cl:store-value    (v) nil)
  (define cl:use-value      (v) nil)
 ) ; macrolet

(labels (
  (visible-p (restart condition)
      (declare (type restart restart))
      (declare (type condition condition))
    (let ((testfn (slot-value restart 'test-function)))
      (or (null testfn) (funcall testfn condition)) ) )
  )
  (defun cl:compute-restarts (&optional condition)
      (declare (values list))
      (declare (type (or condition null) condition))
    (let ((restarts '()))
      (dolist (cluster *restart-clusters* (nreverse restarts))
        (when (or (null condition) (eq (first cluster) condition))
          (dolist (restart (rest cluster))
            (when (or (null condition) (visible-p restart condition))
              (pushnew restart restarts) ) )) ) ) )

  (defun cl:find-restart (id &optional condition)
      (declare (values (or restart null)))
      (declare (type restart-designator id))
      (declare (type (or condition null) condition))
    (dolist (cluster *restart-clusters*)
      (when (or (null condition) (eq (first cluster) condition))
        (dolist (restart (cdr cluster))
          (when (and (or (eq restart id) (eq (restart-name restart) id))
                     (or (null condition) (visible-p restart condition)) )
            (return-from find-restart restart) ) )) ) )
 ) ; labels

(defun cl:invoke-restart (name &rest args)
    (declare (type restart-designator name))
    (declare (dynamic-extent args))
  (apply #'invoke-restart-internal name nil t args) )
