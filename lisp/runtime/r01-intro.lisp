;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 01 Introduction
;;; runtime/r01-intro.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r01-intro.lisp#2 $
;;;
;
(in-package #:si)

;;; [%]
(defun %defconstant (name value doc-string)
    (declare (values symbol))
    (declare (type symbol name))
    (declare (type (or string null) doc-string))
    (declare (ignore doc-string))
  (let ((cell (intern-value-cell name :constant)))
    (setf (slot-value cell 'kind)  :constant)
    (setf (slot-value cell 'value) value)
    (setf (gethash name (slot-value *environment* 'variables))
      `(:constant . ((:constant . ,value))) )
    name ) )

(defun %defvar (name value doc-string initp)
    (declare (values symbol))
    (declare (type symbol name))
    (declare (type (or string null) doc-string))
    (declare (ignore doc-string))
  (let ((cell (intern-value-cell name :special)))
    (when initp (setf (slot-value cell 'value) value))
    (setf (gethash name (slot-value *environment* 'variables))
      `(:variable . ((:special . ,cell))) )
    name ) )

;;; [F]
(defun find-setf-cell (name)
    (declare (values (or setf-cell null)))
    (declare (type symbol name))
  (values (gethash name *setf-table*)) )

(defun find-value-cell (name)
    (declare (values (or tlv-record value-cell null)))
    (declare (type symbol name))
  (values (gethash name *value-table*)) )

;;; [I]
(defun intern-setf-cell (name)
    (declare (values setf-cell))
    (declare (type symbol name))
  (or (gethash name *setf-table*)
    (let ((cell (allocate-record (get-classd 'setf-cell))))
        (setf (slot-value cell 'name) name)
        (setf (gethash name *setf-table*) cell) ) ) )

(defun intern-value-cell (name kind)
    (declare (values (or tlv-record value-cell)))
    (declare (type symbol name))
    (declare (type (member :constant :special) kind))
  (or (gethash name *value-table*)
    (let ((cell (make-instance 'value-cell)))
      (setf (slot-value cell 'name) name)
      (setf (slot-value cell 'kind) kind)
      (setf (slot-value cell 'value) (unbound-marker))
      (setf (gethash name *value-table*) cell) )) )
