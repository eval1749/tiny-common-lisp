;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 05 Data and control Flow
;;; runtime/m05-data.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/macro/m05-data.lisp#2 $
;;;
;
(in-package #:c)

;;; [D]
(defmacro cl:defconstant (name value &optional doc-string)
 `(si::%defconstant ',name ,value ,doc-string) )

(defmacro cl:defvar (name value &optional doc-string)
 `(progn
    (if (boundp ',name)
        (si::%defvar ',name nil ,doc-string nil)
      (si::%defvar ',name ,value ,doc-string t) )) )

;;; [P]
(defmacro cl:psetq (&whole form &rest pair*)
  (let ((runner pair*))
    (loop
      (when (endp runner) (return `(psetf ,@pair*)))
      (let ((var (pop runner)))
        (unless (symbolp var)
          (error "Expect variable name but ~S." var) )
        (when (endp runner)
          (error "Missing value for ~S." var) )
        (pop runner) )) ) )

;;; [S]

#|
    (shiftf a b new)
    =>
    (let ((old-a a))
      (let ((new-a
              (let ((old-b b))
                (let ((new-b new)) (setq b new-b))
                old-b ) ))
        (setq a new-a) )
      old-a )
|#
(defmacro cl:shiftf (place place/new-value &rest place/new-value*
                        &environment env )
  (labels (
    ;; (generate nil new-value) = new-value
    ;; (generate (x x*) new-value) =
    ;;    (let ((old (read-form)))
    ;;      (let ((new generate x* new-value))
    ;;        (write-form) )
    ;;      old )
    (generate (info* new-value)
      (if (null info*)
          new-value
        (let*((info (pop info*))
              (stores (pop info))
              (writer (pop info))
              (reader (pop info)) )
          (if (rest stores)
             `(multiple-value-bind ,stores ,reader
                (multiple-value-bind ,stores ,(generate info* new-value)
                    ,writer )
                (values ,@stores) )
            (let ((store (first stores)))
             `(let ((,store ,reader))
                (let ((,store ,(generate info* new-value)))
                  ,writer )
                ,store ) )) )) )
    )
  (let* ((work (nreverse (list* place place/new-value place/new-value*)))
         (new-value (pop work))
         (place+ (nreverse work)) )
     (si::with-collector (collect-subform subform*)
     (si::with-collector (collect-info info*)
       (dolist (place place+)
         (multiple-value-bind (vars vals stores writer reader)
            (get-setf-expansion place env)
          (dolist (var vars)
            (collect-subform (list var (pop vals))) )
          (collect-info (list stores writer reader)) ) )
       (let ((subform* (subform*)))
         (if subform*
             `(let ,subform* ,(generate (info*) new-value))
           (generate (info*) new-value) ) ) ) ) ) ) )
