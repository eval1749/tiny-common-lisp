;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 05 Data and control Flow
;;; runtime/m05-control.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m05-control.lisp#1 $
;;;
;
(in-package #:c)

(defmacro cl:etypecase (keyform &rest clause*)
  (let ((val (gensym)))
   `(let ((,val ,keyform))
      (typecase ,val
        ,@clause*
        (otherwise
          (error 'type-error
            :datum ,val
            :expected-type '(or ,@(mapcar #'first clause*)) ) )) ) ) )
