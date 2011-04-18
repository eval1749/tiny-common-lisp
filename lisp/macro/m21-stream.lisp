;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 21 Streams
;;; macro/m21-stream.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m21-stream.lisp#1 $
;;;
;
(in-package #:c)

(defmacro cl:with-open-file ((stream filespec &rest option*) &body body)
  (let ((abort (gensym "abort")))
    (multiple-value-bind (decl* form*) (analyze-body body nil)
     `(let ((,stream (open ,filespec ,@option*))
            (,abort t) )
          (declare (type (or stream null) ,stream))
          ,@decl*
        (unwind-protect
            (multiple-value-prog1 (progn ,@form*) (setq ,abort nil))
          (close ,stream :abort ,abort) ) ) ) ) )
