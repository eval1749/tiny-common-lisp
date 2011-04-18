;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 14 Conses
;;; runtime/r14-list.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r14-cons.lisp#7 $
;;;
;
(in-package #:si)

;;; [A]
(defun cl:acons (key val alist)
  (cons (cons key val) alist) )

(defun cl:append (&rest lists)
    (declare (dynamic-extent lists))
  (when lists
    (let* ((anchor (list 0))
           (tail   anchor)
           (list-runner lists) )
      (loop
        (let ((runner (pop list-runner)))
          (when (endp list-runner)
            (setf (cdr tail) runner)
            (return (cdr anchor)) )
          (loop
            (when (endp runner) (return))
            (setq tail (setf (cdr tail) (list (pop runner)))) ) )) )) )

(defun append/2 (list-1 list-2)
    (declare (type list list-1))
    (declare (type t list-2))
    (declare (values t))
  (if (null list-1)
      list-2
    (let* ((head (list (first list-1)))
           (tail head) )
      (dolist (elt (rest list-1))
        (setq tail (setf (rest tail) (list elt))) )
      (setf (rest tail) list-2)
      head )) )

(defun cl:assoc (item alist &key (key #'identity) test test-not)
    (declare (type proper-list alist))
  (let ((test (ensure-test-function test test-not)))
    (dolist (pair alist)
      (when (funcall test (funcall key (car pair)) item)
        (return pair) ) ) ) )

(defun cl:assoc-if (pred alist &key (key #'identity))
    (declare (type function-designator pred))
    (declare (type proper-list alist))
  (let ((test (ensure-test-function test test-not)))
    (dolist (pair alist)
      (when (funcall pred (funcall key (car pair)))
        (return pair) ) ) ) )

(defun assq (item alist)
    (declare (values (or cons null)))
    (declare (type proper-list alist))
  (dolist (pair alist)
    (when (eq (car pair) item) (return pair)) ) )

;;; [L]
(defun cl:last (list &optional (n 1))
    (declare (values t))
    (declare (type list list))
    (declare (type sequence-index n))
  (let ((runner list)
        (result list)
        (i 0) )
    (loop
      (when (atom runner) (return result))
      (when (>= i n) (pop result))
      (setq runner (cdr runner))
      (incf i) ) ) )

;;; [M]
;;; Implemenation of
;;;     mapc    mapc/1    ... mapc/3
;;;     mapcar  mapcar/1  ... mapcar/3
;;;     mapc    mapc/1    ... mapc/3
;;;     mapcan  mapcan/1  ... mapcan/3
;;;     mapl    mapl/1    ... mapl/3
;;;     maplist maplist/1 ... maplist/3
;;;     mapcon  mapcon/1  ... mapcon/3
;;;
;;; See compiler/tinycl_c_init.cpp for variable arity optimization.
(macrolet (
  (collect-nothing (&rest form*)
    `(macrolet ((collect (form) form)) ,@form* list-1) )

  (collect-item (&rest form*)
   `(macrolet (
      (collect (form) `(setq tail (setf (cdr tail) (list ,form))))
      )
     (let* ((head (list 0))
            (tail head) )
       ,@form*
       (cdr head) ) ) )

  (collect-list (&rest form*)
   `(macrolet (
      (collect (form)
       `(let ((list ,form))
          (setf (cdr tail) list)
          (setq tail (last list)) ) )
      )
      (let* ((head (list 0))
             (tail head) )
        ,@form*
        (cdr head) ) ) )

  (define (name collector itemize)
    (labels (
      (list-names (format count)
        (with-collector (collect result)
          (dotimes (nth count (result))
            (collect (intern (format nil format (+ nth 2)))) ) ) )

      (make-defun/n (name param* arg*)
       `(defun ,name (fn list-1 ,@param*)
            (declare (values list))
            (declare (type (or function symbol) fn))
            (declare (type list list-1 ,@param*))
          (,collector
            (let ((runner-1 list-1)
                  ,@(mapcar/2 (lambda (x y) (list x y)) arg* param*) )
              (loop
                (when (or (endp runner-1)
                      ,@(mapcar/1 (lambda (x) `(endp ,x)) arg*) )
                  (return) )
                (collect
                  (funcall fn
                    (,itemize runner-1)
                    ,@(mapcar/1 (lambda (x) (list itemize x)) arg*) ))
                (setq runner-1 (cdr runner-1))
                ,@(mapcar/1 (lambda (x) `(setq ,x (cdr ,x))) arg*) ) ) ) ) )

      (make-defun ()
       `(defun ,name (fn list-1 &rest list*)
            (declare (values list))
            (declare (type (or function symbol) fn))
            (declare (type list list-1))
            (declare (dynamic-extent list*))
          (if (null list*)
              (,(intern (format nil "~A/1" name)) fn list-1)
            (labels (
              (endp+ (list+)
                (dolist (list list+ nil)
                  (when (endp list) (return t)) ) )
              )
              (,collector
                (let ((list+ (cons list-1 list*)))
                  (loop
                    (when (endp+ list+) (return))
                    (let ((arg*
                            (with-collector (collect result)
                              (let ((runner list+))
                                (loop
                                  (when (endp runner) (return (result)))
                                  (collect (car runner))
                                  (setf (car runner) (cdr (car runner)))
                                  (setq runner (cdr runner)) ) ) ) ))
                      (collect (apply fn arg*)) )) )) )) ) )

      (mapcar/1 (fn list-1)
          (declare (values list))
          (declare (type list list-1))
        (with-collector (collect result)
          (let ((runner-1 list-1))
            (loop
             (when (endp runner-1) (return (result)))
             (let ((item-1 (car runner-1)))
               (collect (funcall fn item-1)) )
             (setq runner-1 (cdr runner-1)) ) ) ) )

      (mapcar/2 (fn list-1 list-2)
          (declare (values list))
          (declare (type list list-1 list-2))
        (with-collector (collect result)
          (let ((runner-1 list-1) (runner-2 list-2))
            (loop
              (when (or (endp runner-1) (endp runner-2)) (return (result)))
              (let ((item-1 (car runner-1))
                    (item-2 (car runner-2)) )
                (collect (funcall fn item-1 item-2)) )
              (setq runner-1 (cdr runner-1))
              (setq runner-2 (cdr runner-2)) ) ) ) )
      )
      ;;
      (with-collector (collect result)
        (dotimes (nth 3)
          (let ((name (intern (format nil "~A/~D" name (+ nth 1)))))
            (collect `(declaim (ftype
                        (function ((or function symbol)
                                   list ,@(list-names "LIST" nth) )
                                  list )
                        ,name )))
            (collect
              (make-defun/n
                  name
                  (list-names "LIST-~D" nth)
                  (list-names "RUNNER-~D" nth) )) ) )
        (collect (make-defun))
        `(progn ,@(result)) ) ) )
  )
  ;;
  (define cl:mapc   collect-nothing car)
  (define cl:mapcar collect-item    car)
  (define cl:mapcan collect-list    car)

  (define cl:mapl    collect-nothing identity)
  (define cl:maplist collect-item    identity)
  (define cl:mapcon  collect-list    identity)
 ) ; macrolet

;;; [N]

(defun nconc/2 (list-1 list-2)
  (if (endp list-1)
      list-2
    (let ((scan-1 list-1))
      (loop
        (let ((next-1 (rest scan-1)))
          (when (null next-1)
            (setf (rest scan-1) list-2)
            (return list-1) )
          (setq scan-1 next-1) )) )) )

(defun cl:nconc (&rest lists)
    (declare (dynamic-extent lists))
  (cond
    ((null lists) '())
    ((null (cdr lists)) (car lists))
    (t
      (let ((list1 (car lists)))
        ;; Skip leading nil's
        (loop
          (setq lists (cdr lists))
          (unless (null list1) (return))
          (setq list1 (car lists))
          (when (null lists) (return)) )

        (unless (null lists)
          (let ((last (last list1)))
            (loop
              (let ((list (car lists)))
               (setf (cdr last) list)
               (setq lists (cdr lists))
               (when (null lists) (return))
               (when list (setq last (last list))) )) ))
         list1 ) )) )

(defun cl:nth (n list)
    (declare (values t))
    (declare (type sequence-index n))
    (declare (type list list))
  (car (nthcdr n list)) )

(defun (setf cl:nth) (new n list)
    (declare (values t))
    (declare (type sequence-index n))
    (declare (type list list))
  (setf (car (nthcdr n list)) new) )

(defun cl:nthcdr (n list)
    (declare (values t))
    (declare (type sequence-index n))
    (declare (type list list))
  (let ((runner list))
    (loop
      (when (zerop n) (return runner))
      (when (endp runner) (return nil))
      (decf n)
      (setq runner (cdr runner)) ) ) )

;;; [P]
(defun cl:pairlis (keys vals &optional alist)
  (loop
    (when (and (endp keys) (endp vals))
      (return alist) )
    (push (cons (pop keys) (pop vals)) alist) ) )

;;; [S]

;;; Native implementation of merge-sort. This function causes stack overflow
;;; for long list.
(defun stable-sort/list (list predicate key)
  (labels (
    (merge (left right)
      (with-collector (collect result)
        (loop
          (unless (and left right) (return))
           (if (funcall predicate (funcall key (first left))
                          (funcall key (first right)) )
               (collect (pop left))
             (collect (pop right)) ))
        (loop
          (when (endp left) (return))
          (collect (pop left)) )
        (loop
          (when (endp right) (return))
          (collect (pop right)) )
        (result) ) )

    (merge-sort (m)
      (if (null (cdr m))
          m
        (let ((left '())
              (right '()) )
          (loop
            (when (endp m) (return))
            (push (pop m) left)
            (when (endp m) (return))
            (push (pop m) right) )
          (merge (merge-sort left) (merge-sort right)) )) )
    )
    (merge-sort list) ) )
