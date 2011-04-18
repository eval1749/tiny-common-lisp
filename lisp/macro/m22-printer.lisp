;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 22 - Printers
;;; macro/m22-printer.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m22-printer.lisp#1 $
;;;
;
(in-package #:c)

(defmacro cl:print-unreadable-object ((object stream &key identity type)
                                        &rest form* )
  (if (null form*)
     `(si::print-unreadable-object-function
        ,object ,stream ,type ,identity nil )
    (let ((fn '#:|print-unreadable-object|))
     `(flet (
        (,fn (,object ,stream)
            (declare (ignorable ,object ,stream))
          (progn ,@form*) )
        )
        (si::print-unreadable-object-function
            ,object ,stream ,type ,identity #',fn ) ) )) )
