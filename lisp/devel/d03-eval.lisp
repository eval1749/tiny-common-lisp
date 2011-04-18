;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Devel - 3 Evaluation and Compilation
;;; lisp/devel/d03-eval.lisp
;;;
;;; Copyright (C) 1996-2009 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/devel/d03-eval.lisp#2 $
;;;
;
(in-package #:c)

(defun cl:compile (name &optional definition)
  (unless (or (null name) (typep name 'function-name))
    (error "Bad function name ~S" name) )
  (multiple-value-bind (fn warnings-p failure-p)
      (cond
        ((functionp definition)
          (values definition nil nil) )
        ((and (consp definition)
              (eq (first definition) 'lambda) )
          (multiple-value-bind (fn warnings failures)
              (compile-form definition)
            (unless fn (error "Compilation failed."))
            (values (funcall fn) warnings failures) ) )
        ((and name (null definition))
          (unless (fboundp name)
            (error 'undefined-function :name name) )
          (values nil nil nil) )
        (t
          (error "Expect lambda expression or function: ~S" definition) ))
    (when (and name definition fn)
      (if (and (fboundp name) (macro-function name))
          (setf (macro-function name) fn)
        (setf (fdefinition name) fn) ))
    (values (or name fn) warnings-p failure-p) ) )

(defun eval (form)
  (labels (
    (eval-args (args)
      (mapcar #'eval args) )

    (eval-compound-form (form op)
        (declare (type symbol op))
      (multiple-value-bind (kind localp)
          (si::function-information op)
        (when localp
          (error "Can't use local function ~S." op) )
        (case kind
          ((:macro)
            (eval (macroexpand-1 form)) )
          ((:special-operator)
            (case op
              ((eval-when) (eval/eval-when form))
              ((function) (eval/function form))
              ((if) (eval/if form))
              ((load-time-value) (eval/load-time-value form))
              ((progn) (eval-forms (cdr form)))
              ((quote) (eval/quote form))
              ((setq) (eval/setq form))
              (otherwise
                (funcall
                    (compile nil
                        `(lambda ()
                            (declare (function-name :toplevel))
                          ,form ))) )) )
          (otherwise
            (apply op (eval-args (rest form))) )) ) )

    (eval-cons (form)
      (let ((op (first form)))
        (cond
          ((symbolp op) (eval-compound-form form op))
          ((and (consp op) (eq (first op) 'lambda))
            (apply (compile nil op) (eval-args (rest form))) )
          (t (error "Invalid operator: ~S" op)) ) ) )

    (eval-forms (forms)
      (loop
        (let ((form (pop forms)))
          (when (null forms) (return (eval form)))
          (eval form) )) )

    (eval-symbol (symb)
      (multiple-value-bind (kind localp alist)
          (si::variable-information symb)
        (when localp
          (error "Can't use local variable ~S." symb) )
        (case kind
          ((:constant)
            (cdr (assoc :constant alist)) )
          ((:symbol-macro)
            (values (eval (cdr (assoc :symbol-macro alist)))) )
          (otherwise
            (symbol-value symb) )) ) )

    (eval/eval-when (form)
      (labels (
        (execute-p (situation*)
          (or (member :execute situation*)
              (member 'eval    situation*)
              (when (eq c::*situation* 'load)
                    (or (member :load-toplevel situation*)
                        (member 'load          situation*) ))
              (when (eq c::*processing-mode* :compile-time-too)
                    (or (member :compile-toplevel situation*)
                        (member 'compile          situation*) ))) )
        )
        (when (execute-p (second form))
          (eval-forms (cddr form)) ) ) )

    (eval/function (form)
      (let ((fname (second form)))
        (cond
          ((or (symbolp fname)
               (and (consp fname) (eq (first fname) 'setf)))
            (multiple-value-bind (kind localp)
                (si::function-information fname)
              (when localp
                (error "Can't use local function ~S" fname) )
              (ecase kind
                ((nil :function)
                  (fdefinition fname) )
                ((:macro)
                  (error "~S is macro." fname) )
                ((:special-operator)
                  (error "~S is special-operator." fname) )) ) )
            ((and (consp fname) (eq (first fname) 'lambda))
              (compile nil fname) )
            (t
              (error "Invalid function name ~S" fname) )) ) )

    (eval/if (form)
      (eval (if (eval (second form)) (third form) (fourth form))) )

    (eval/load-time-value (form)
      (eval (second form)) )

    (eval/quote (form)
      (second form) )

    (eval/setq (form)
      (let ((runner (rest form))
            (value nil) )
        (loop
          (when (null runner) (return value))
          (unless (consp runner) (malformed-form form))
          (let ((name (pop runner)))
            (unless (symbolp name)
              (error "Invalid variable name ~S" name) )
            (when (null runner)
              (error "Missing value form for ~S." name) )
            (unless (consp runner) (malformed-form form))
            (multiple-value-bind (kind localp alist)
                (si::variable-information name)
              (when localp (error "Can't use local variable ~S" name))
              (ecase kind
                ((:special nil)
                  (setq value (eval (pop runner)))
                  (setf (symbol-value name) value) )
                ((:constant)
                  (error "Can't alter constant ~S" name) )
                ((:symbol-macro)
                  (let ((expansion (cdr (assoc kind alist)))
                        (value-form (pop runner)) )
                    (setq value
                      (eval `(setf ,expansion ,value-form))) ) )) ) )) ) )

    (malformed-form (form)
      (error "Malformed form ~S" form) )
    )
    (typecase form
      (symbol (eval-symbol form))
      (cons   (eval-cons form))
      (otherwise form) ) ) )


(defun cl:macroexpand (form &optional env)
  (multiple-value-bind (expansion expanded-p) (macroexpand-1 form env)
    (if (not expanded-p)
        (values expansion nil)
      (let ((runner expansion))
        (loop
          (multiple-value-bind (expansion expanded-p)
              (macroexpand-1 runner env)
            (unless expanded-p (return (values expansion t)))
            (setq runner expansion) )) )) ) )

(defun cl:macroexpand-1 (form &optional env)
    (declare (values t t))
  (typecase form
    (cons
      (let ((expander
              (when (symbolp (first form))
                (macro-function (first form)) ) ))
        (values (if expander
                    (funcall *macroexpand-hook* expander form env)
                  form )
                expander ) ) )
    (symbol
      (multiple-value-bind (kind localp alist)
          (si::variable-information form env)
          (declare (ignore localp))
        (if (eq kind :symbol-macro)
            (values (cdr (assoc kind alist)) t)
          (values form nil) ) ) )
    (otherwise
      (values form nil) )) )

#+nil
(defun cl:macro-function (symb &optional env)
    (declare (values (or function null)))
    (declare (type symbol symb))
  (cdr (assoc :macro (nth-value 2 (si::function-information symb env)))) )


(defun cl:special-operator-p (symb)
    (declare (type symbol symb))
  (eq (si::function-information symb nil) :special-operator) )
