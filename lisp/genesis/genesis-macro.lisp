;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Macros
;;; genesis/genesis-macro.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis-macro.lisp#6 $
;;;
;
(in-package #:si)

;;; for defmacro and distructuring-bind
(defun check-keywords (keys keyvals)
  (let ((bad-keys '())
        (invalid-keys '()) )
    (loop
      (unless (consp keyvals)
        (when bad-keys
          (error 'unrecognized-keyword-argument
            :datum (first bad-keys)
            :expected-type `(member ,@keys) ))
        (when invalid-keys
          (error 'invalid-keyword-argument
            :datum (first invalid-keys)
            :expected-type `(member ,@keys) ))
        (return) )
      (let ((key (pop keyvals)))
       (when (eq key :allow-other-keys) (return))
       (cond
         ((not (symbolp key)) (push key invalid-keys))
         ((memq key keys) (push key bad-keys)) )
       (pop keyvals) )) ) )

;;; for defmacro and distructuring-bind
(defun key (key list)
    (declare (values list))
  (loop
    (unless (consp list) (return nil))
    (when (eq (car list) key) (return list))
    (setq list (cddr list)) ) )

;;; for mapX
(defmacro with-collector ((collect &optional result) &body form*)
  (if result
      (let ((head (gensym))
            (tail (gensym)) )
       `(let* ((,head (list 0))
               (,tail ,head) )
            (declare (type cons ,head ,tail))
          (macrolet (
            (,collect (form) `(setq ,',tail (setf (cdr ,',tail) (list ,form))))
            (,result () '(cdr ,head))
            )
            ,@form* ) ) )
    (let ((head (gensym))
          (tail (gensym)) )
     `(let* ((,head (list 0))
             (,tail ,head) )
          (declare (type cons ,head ,tail))
        (macrolet (
          (,collect (form) `(setq ,',tail (setf (cdr ,',tail) (list ,form))))
          )
         ,@form* (cdr ,head) ) ) )) )

(defmacro cl:ecase (keyform &rest clause*)
  (labels (
    (collect-all-keys ()
      (let ((all-keys '()))
        (dolist (clause clause* (nreverse all-keys))
          (let ((keys (first clause)))
            (if (consp keys)
                (dolist (key keys) (push key all-keys))
              (push keys all-keys) ) ) ) ) )
    )
    (let ((keys (collect-all-keys))
          (keyvar '#:key) )
     `(let ((,keyvar ,keyform))
        (case ,keyvar
          ,@clause*
          (otherwise
            (error 'case-failure
              :datum ,keyvar
              :expected-type '(member ,@keys) )) ) ) ) ) )

(defmacro cl:with-input-from-string ((var string &key index (start 0) end)
                                     &body body )
  (if (not index)
      `(let ((,var (make-string-input-stream ,string ,start ,end)))
         ,@body )
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
        (multiple-value-prog1 (locally ,@body)
          (setf ,index (slot-value ,var 'si::index)) ) )) )

(defmacro cl:with-output-to-string
    ((var &optional string-form &rest args &key element-type) &body body)
        (declare (ignore element-type))
  (if string-form
      `(let* ((,var (make-instance 'si::string-output-stream
                        :flags 2
                        :string ,string-form
                        ,@args )) )
           ,@body )
  `(let* ((,var (make-string-output-stream ,@args)))
      ,@body 
      (get-output-stream-string ,var) ) ) )
