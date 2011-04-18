;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 01 Introduction
;;; macro/m01-intro.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m01-intro.lisp#1 $
;;;
;
(in-package #:c)

;;; <summary>
;;;   Analyzes body.
;;; </summary>
;;; <returns>
;;;   <list>
;;;     <item><term>declarations</term></item>
;;;     <item><term>forms</term></item>
;;;     <item><term>document string or nil</term></item>
;;;   </list>
;;; </returns>
(defun analyze-body (body doc-p)
  (si::with-collector (collect result)
    (let ((runner body)
          (docstr nil) )
      (loop
        (when (endp runner) (return))
        (let ((form (first runner)))
          (cond
            ((consp form)
              (unless (eq (first form) 'declare) (return))
              (collect form) )
            ((and doc-p (stringp form))
              (when docstr (return))
              (setq docstr form) )
            (t
              (return) ))
          (setq runner (cdr runner)) ))
      (values (result) runner docstr) ) ) )
