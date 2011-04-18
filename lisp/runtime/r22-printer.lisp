;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 22 Printer
;;; runtime/r22-printer.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r22-printer.lisp#6 $
;;;
;
(in-package #:si)

;;; [P]

(defun cl:pprint (obj &optional s)
    (declare (values t))
    (declare (type output-stream-designator s))
  (let ((s (ensure-output-stream s)))
    (terpri s)
    (let ((*print-pretty* t))
      (prin1 obj s) )
    obj ) )

(defun cl:prin1 (obj &optional s)
    (declare (values t))
    (declare (type output-stream-designator s))
  (let ((s (ensure-output-stream s)))
    (let ((*print-escape* t))
      (print-object obj s) ) ) )

(defun cl:prin1-to-string (obj)
    (declare (values string))
  (with-output-to-string (s) (prin1 obj s)) )

(defun cl:princ (obj &optional s)
    (declare (values t))
    (declare (type output-stream-designator s))
  (let ((s (ensure-output-stream s)))
    (let ((*print-escape* nil)
          (*print-readably* nil) )
      (print-object obj s) ) ) )

(defun cl:princ-to-string (obj)
    (declare (values string))
  (typecase obj
    (string obj)
    (symbol (symbol-name obj))
    (otherwise (write-to-string obj :escape nil)) ) )

(defun cl:print (obj &optional s)
    (declare (values t))
    (declare (type output-stream-designator s))
  (let ((s (ensure-output-stream s)))
    (terpri s)
    (prin1 obj s)
    (write-char #\Space s)
    obj ) )

(defun print-unreadable-object-function (object stream type idenity fn)
    (declare (values null))
    (declare (type stream stream))
    (declare (type (or function null) fn))
  (when *print-readably*
    (error 'print-not-readable :object object) )

  (write-string "#<" stream)

  (when type
    (format stream "~:(~A~)" (class-name (class-of object))) )

  (when fn
    (when type (write-char #\Space stream))
    (funcall fn object stream) )

  (when idenity
    (when (or fn type) (write-char #\Space stream)) )

  ;; Note: Avoid bignum operation.
  #+64bit
  (format stream "@ ~X>" (address-of object))
  #-64bit
  (format stream "@ ~X~X>"
        (ash (address-of object) -4)
        (ash (logand (address-of object) 15) -4) )
  nil )

;;; [W]

(defun cl:write (obj &key
    ((:array    *print-array*)      *print-array*)
    ((:base     *print-base*)       *print-base*)
    ((:case     *print-case*)       *print-case*)
    ((:circle   *print-circle*)     *print-circle*)
    ((:escape   *print-escape*)     *print-escape*)
    ((:gensym   *print-gensym*)     *print-gensym*)
    ((:length   *print-length*)     *print-length*)
    ((:level    *print-level*)      *print-level*)
    ((:lines    *print-lines*)      *print-lines*)
    ((:miser-width  *print-miser-width*)    *print-miser-width*)
    ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
    ((:pretty   *print-pretty*)     *print-pretty*)
    ((:radix    *print-radix*)      *print-radix*)
    ((:readably *print-readably*)   *print-readably*)
    ((:right-margin *print-right-margin*) *print-right-margin*)
    stream )
  (let ((s (ensure-output-stream stream)))
    (print-object obj s) ) )

(defun cl:write-to-string (obj &rest keyvals)
    (declare (values t))
  (with-output-to-string (s) (apply #'write obj :stream s keyvals)) )
