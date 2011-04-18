;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - CLOS
;;; genesis/genesis-clos.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis-clos.lisp#3 $
;;;
;
(in-package #:si)

;; for deubgging
(defun cl:slot-missing (class object slot-name operation &optional new-value)
    (declare (ignore class operation new-value))
  (error "~S doesn't have a slot ~S." object slot-name) )

(defun %defgeneric (fname lambda-list &rest initargs)
    (declare (values generic-function))
    (declare (type function-name fname))
    (declare (type list lambda-list))
    (declare (ignore initargs))
    (declare (dynamic-extent initargs))
  (labels (
    (make-finfo ()
      (multiple-value-bind (reqs opts rest key keys auxs)
          (analyze-lambda-list lambda-list)
        (when auxs
          (error "Can't use &aux for generic-function lambda-list.") )
        (let ((finfo (allocate-record (get-classd 'function-information))))
          (setf (slot-value finfo 'max-params)
            (+ (length reqs) (length opts)) )
          (setf (slot-value finfo 'min-params) (length reqs))
          (setf (slot-value finfo 'keys) (and key (cons key keys)))
          (setf (slot-value finfo 'rest) rest)
          finfo ) ) )

    (make-method-cache ()
      (let* ((line-size 2)
             (nentries (* line-size 50))
             (order (if (consp fname) #(1) #(0))) )
        (let ((mc (allocate-record (get-classd 'method-cache))))
          (setf (slot-value mc 'count) 0)
          (setf (slot-value mc 'default-emf) nil)
          (setf (slot-value mc 'eql-caches) nil)
          (setf (slot-value mc 'line-size) line-size)
          (setf (slot-value mc 'order) order)
          (setf (slot-value mc 'vector) (make-vector nentries))
          mc ) ) )
    )
    (let ((fn (and (fboundp fname) (fdefinition fname))))
      (when (and fn (not (typep fn 'generic-function)))
        (error 'generic-clobbers-function :name fname) ) )
    (let* ((finfo (make-finfo))
           (method-cache (make-method-cache))
           (gf (make-instance 'standard-generic-function
                  :lambda-list
                    lambda-list
                  :method-class
                    (find-class 'standard-method)
                  :method-combination
                    +standard-method-combination+
                  :name
                    fname ) ))
      (setf (slot-value gf 'function-information) finfo)
      (setf (slot-value gf 'method-cache) method-cache)
    (set-funcallable-instance-function gf (compute-discriminator gf))
    (setf (fdefinition fname) gf)
    gf ) ) )

(defun %defmethod (fname qualifiers lambda-list specializers function
                    &rest initargs )
    (declare (type function-name fname))
    (declare (type function function))
    (declare (type list lambda-list))
    (declare (type list qualifiers))
    (declare (type list specializers))
  (labels (
    (add-method (gf mt)
        (declare (values generic-function))
        (declare (type generic-function gf))
        (declare (type method mt))
      (labels (
        (match-p (mt-1 mt-2)
          (and (equal (slot-value mt-1 'qualifiers)
                      (slot-value mt-2 'qualifiers) )
               (equal (slot-value mt-1 'specializers)
                      (slot-value mt-2 'specializers) )) )
        )
        (setf (slot-value mt 'generic-function) gf)
        (let* ((methods (slot-value gf 'methods))
               (runner  methods) )
          (loop
            (when (endp runner)
              (setf (slot-value gf 'methods) (cons mt methods))
              (return gf) )
            (when (match-p (first runner) mt)
              (setf (first runner) mt)
              (return gf) )
            (setq runner (cdr runner)) ) ) ) )
    )
  (let* ((gf (fdefinition fname))
         (mt (apply #'make-instance
                (slot-value gf 'method-class)
                :function           function
                :lambda-list        lambda-list
                :qualifiers         qualifiers
                :specializers       specializers
                initargs ) ))
    (add-method gf mt)
    mt ) ) )

(defun compute-effective-method-using-arguments (gf args)
    (declare (values function))
    (declare (type generic-function gf))
    (declare (type list args))
  (let ((method (first (std-compute-applicable-methods gf args))))
    (if method
        (slot-value method 'function)
      (lambda (&rest args) (apply 'no-applicable-method gf args)) ) ) )

(defun std-compute-applicable-methods (gf args)
    (declare (values list))
    (declare (type standard-generic-function gf))
    (declare (type list args))
  (labels (
    (compute ()
      (with-collector (collect result)
        (dolist (method (slot-value gf 'methods)
                    (sort (result) #'more-specific-p) )
          (when (match-p args method)
            (collect method) ) ) ) )

    (match-p (args method)
      (dolist (specializer (slot-value method 'specializers) t)
        (when (null args) (return nil))
        (unless (typep (pop args) specializer)
          (return nil) ) ) )

    (more-specific-p (method-1 method-2)
        (declare (type method method-1 method-2))
      (let ((runner-1 (slot-value method-1 'specializers))
            (runner-2 (slot-value method-2 'specializers)) )
        (loop
          (when (endp runner-1) (return t))
          (when (endp runner-2) (error "Broken method ~S" method-2))
          (let ((specializer-1 (pop runner-1))
                (specializer-2 (pop runner-2)) )
            (unless (eq specializer-1 specializer-2)
              (return (subclassp specializer-1 specializer-2)) ) )) ) )
    )
    (compute) ) )

(defun cl:no-applicable-method (gf &rest args)
  (error "There is no applicable method for ~S with ~S." gf args) )
