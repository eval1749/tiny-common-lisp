;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 60 Document Object Model
;;; runtime/r60-dom-sexp.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r60-dom-sexp.lisp#1 $
;;;
;
(in-package :dom)

(defun %qname (obj)
  (string obj) )

(defun %append-element (parent name)
  (let ((elt (make-element (owner-document parent) (%qname name))))
    (append-child parent elt) ) )

(defun %append-object (parent obj)
  (let ((doc (owner-document parent)))
    (append-child parent (make-text doc (princ-to-string obj))) ) )

(defun %set-attribute (element name value)
  (setf (attribute element (%qname name)) (princ-to-string value)) )

(defmacro document (&rest token*)
  (labels (
    (parse-element (token*)
      (let ((runner (rest token*)))
        (si::with-collector (collect program)
          ;; Process attributes
          (loop
            (when (endp runner) (return))
            (let ((token (first runner)))
              (unless (keywordp token) (return))
              (let ((aname (pop runner))
                    (avalue (pop runner)) )
                (collect `(%set-attribute .elt ',aname ,avalue)) ) ))

          ;; Process children
          (loop
            (when (endp runner) (return))
            (let ((token (pop runner)))
              (collect (parse-token token)) ))
          (program) ) ) )

    (parse-token (token)
      (if (consp token)
         `(let ((.elt (%append-element .elt ',(first token))))
            (declare (ignorable .elt))
            ,@(parse-element token) )
        `(%append-object .elt ,token) ) )
    )
   `(let* ((.doc  (make-document (%qname ',(first token*))))
           (.elt  (document-element .doc)) )
        (declare (ignorable .elt))
      ,@(parse-element token*)
      .doc ) ) )
