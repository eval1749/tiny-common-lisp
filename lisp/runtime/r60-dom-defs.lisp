;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 60 Document Object Model
;;; runtime/r60-dom-defs.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r60-dom-defs.lisp#1 $
;;;
;
(in-package :dom)

(defconstant +unspecified-namespace+
    (let ((ns (make-instance 'dom:namespace)))
      (setf (slot-value ns 'expanded-table)  (make-hash-table :test 'equal))
      (setf (slot-value ns 'qualified-table) (make-hash-table :test 'equal))
      (setf (slot-value ns 'uri) "")
      ns ) )

(defgeneric dom:append-child     (parent new-child))

(defgeneric dom:attribute        (node qname &optional namespace-uri))
(defgeneric (setf dom:attribute) (nv node qname &optional ns-uri) )

(defgeneric dom:attribute-node (node qname &optional namespace-uri))

(defgeneric dom:first-child      (node))
(defgeneric dom:insert-after     (parent new-child ref-child))
(defgeneric dom:insert-before    (parent new-child ref-child))
(defgeneric dom:last-child       (node))
(defgeneric dom:next-sibling     (node))
(defgeneric dom:node-name        (node))
(defgeneric dom:node-value       (node))
(defgeneric (setf dom:node-value) (new-value node))
(defgeneric dom:owner-document   (node))
(defgeneric dom:parent-node      (node))
(defgeneric dom:previous-sibling (node))
(defgeneric dom:remove-child     (parent old-child))
(defgeneric dom:replace-child    (parent new-child old-child))
(defgeneric dom:serialize (node stream))
(defgeneric dom:text-content (node stream))
