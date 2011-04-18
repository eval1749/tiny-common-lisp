;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 22 Printer - print-object
;;; runtime/r22-printer.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/devel/d22-print-object.lisp#1 $
;;;
;
(in-package #:si)

;; [R]
(defmethod cl:print-object ((o cl:restart) s)
  (let ((printer
            (if (or *print-readably* *print-escape*)
                nil
              (slot-value o 'report-function) ) ))
    (if printer
        (funcall printer s)
      (print-unreadable-object (o s :identity t :type t)
        (prin1 (slot-value o 'name) s) ))
    o ) )

;; [S]
(defmethod cl:print-object ((o cl:simple-condition) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t))
    (apply #'format s
        (slot-value o 'format-control)
        (slot-value o 'format-arguments) ))
  o )

;; [T]
(defmethod cl:print-object ((o too-few-arguments) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t))
    (format s "~S is called with too few arguments."
        (slot-value o 'function) ))
  o )

(defmethod cl:print-object ((o cl:type-error) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t))
    (format s "Expect type ~S but ~S."
        (slot-value o 'expected-type)
        (slot-value o 'datum) ))
  o )

(defmethod cl:print-object ((o too-many-arguments) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t))
    (format s "~S is called with too many arguments."
        (slot-value o 'function) ))
  o )

;; [U]
(defmethod cl:print-object ((o cl:undefined-function) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t)
        (prin1 (slot-value o 'name)) )
    (format s "Function ~S is undefined." (slot-value o 'name)) )
  o )

(defmethod cl:print-object ((o cl:unbound-variable) s)
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (o s :identity t :type t)
        (prin1 (slot-value o 'name)) )
    (format s "Variable ~S is unbound." (slot-value o 'name)) )
  o )
