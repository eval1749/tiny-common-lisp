;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 10 Symbols
;;; runtime/r10-symbol.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r10-symbol.lisp#5 $
;;;
;
(in-package #:si)

;;; [B]
(defun cl:boundp (name)
    (declare (values t))
    (declare (type symbol name))
  (if (keywordp name)
      name
    (let ((cell (find-value-cell name)))
      (typecase cell
        (tlv-record
          (not (eq (!tlv (slot-value cell 'index)) (unbound-marker))) )
        (value-cell
          (not (eq (slot-value cell 'value) (unbound-marker))) )) )) )

;;; [S]
;;; symbol reader
(macrolet (
  (define (name slot-name)
   `(defun ,name (x)
      (unless (symbolp x) (error 'type-error :datum x :expected-type 'symbol))
      (let ((value (slot-value (the symbol x) ',slot-name)))
       ,(if (eq slot-name 'function)
           '(if (functionp value)
                 value
              (error 'undefined-function :name x))
          'value ) )) )
  )
  (define cl:symbol-function function)
  (define cl:symbol-name     name)
  (define cl:symbol-package  package)
  (define cl:symbol-plist    plist)
 ) ; macrolet

(defun (setf cl:symbol-function) (fn sym)
  (declare (values function))
  (declare (type symbol sym))
  (declare (type function fn))
 (update-callers sym fn)
 (setf (slot-value sym 'function) fn) )

(defun cl:symbol-value (name)
    (declare (values t))
    (declare (type symbol name))
  (if (keywordp name)
      name
    (let ((cell (find-value-cell name)))
      (typecase cell
        (null
          (error 'unbound-variable :name name) )
        (tlv-record
          (!tlv (slot-value cell 'index)) )
        (value-cell
          (let ((value (slot-value cell 'value)))
            (when (eq value (unbound-marker))
              (error 'unbound-variable :name name) )
            value ) )) )) )

(defun (setf cl:symbol-value) (value name)
  (labels (
    (alter-constant (name)
      (error "Can't alter constant ~S" name) )
    )
  (if (keywordp name)
      (alter-constant name)
    (let ((cell (intern-value-cell name :special)))
      (etypecase cell
        (tlv-record
          (setf (!tlv (slot-value cell 'index)) value) )
        (value-cell
          (when (eq (slot-value cell 'kind) :constant)
            (alter-constant name) )
          (setf (slot-value cell 'value) value) )) )) ) )
