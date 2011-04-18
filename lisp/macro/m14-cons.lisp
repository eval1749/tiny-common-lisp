;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Macro - 14 Conses
;;; macro/m14-cons.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m14-cons.lisp#1 $
;;;
;
(in-package #:c)

(defmacro cl:push (item place &environment env)
  (multiple-value-bind (var* val* store* write read)
      (get-setf-expansion place env)
   `(let* ((.item ,item)
           ,@(mapcar #'list var* val*)
           (,(first store*) (cons .item ,read))
           ,@(rest store*) )
      ,write ) ) )

(defmacro cl:pushnew (item place &rest keyval* &environment env)
  (multiple-value-bind (var* val* store* write read)
      (get-setf-expansion place env)
   `(let* ((.item ,item)
           ,@(mapcar #'list var* val*)
           (,(first store*) (adjoin .item ,read ,@keyval*))
           ,@(rest store*) )
      ,write ) ) )
