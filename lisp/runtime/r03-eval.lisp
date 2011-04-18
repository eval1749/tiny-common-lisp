;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 03 Evaluation and Compilation
;;; runtime/r03-eval.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r03-eval.lisp#1 $
;;;
;
(in-package #:si)

;;; <summary>
;;;   Analyzes ordinal lambda list.
;;; </summary>
(defun analyze-lambda-list (lambda-list)
  (let ((auxs '())
        (key  nil)
        (keys '())
        (opts '())
        (reqs '())
        (rest nil)
        (state :required) )
    (labels (
      (add-keyword (spec initform svar)
        (cond
          ((symbolp spec)
            (push (list (intern (symbol-name spec) :keyword)
                        spec
                        initform
                        svar )
                  keys ) )
          ((and (consp spec)
                (symbolp (car spec))
                (consp (cdr spec))
                (symbolp (cadr spec))
                (null (cddr spec)) )
            (push (list (car spec) (cadr spec) initform svar) keys) )
          (t
            (error "Invalid keyword specifier specification: ~S" spec) )) )
      )
    (dolist (token lambda-list
                (values (nreverse reqs)
                        (nreverse opts)
                        rest
                        key
                        (nreverse keys)
                        (nreverse auxs) ))
      (ecase state
        ((:allow-other-keys)
          (cond
            ((eq token '&aux)
              (setq state :aux) )
            (t
              (error "Extra token after &allow-other-keys") )) )

        ((:aux)
          (cond
            ((symbolp token)
              (push (list token) auxs) )
            ((consp token)
              (let* ((runner token)
                     (var      (pop runner))
                     (initform (when (consp runner) (pop runner))) )
                (unless (null runner)
                  (error "Invalid &aux specifier ~S" token) )
                (push (list var initform) auxs) ) )
            (t
              (error "Bad &aux specifier ~S" token) )) )

        ((:key)
          (cond
            ((eq token' &allow-other-keys)
              (setq key '&allow-other-keys)
              (setq state :allow-other-keys) )

            ((eq token '&aux)
              (setq state :aux) )

            ((symbolp token)
              (add-keyword token nil nil) )

            ((consp token)
              (let* ((runner token)
                     (spec     (pop runner))
                     (initform (when (consp runner) (pop runner)))
                     (svar     (when (consp runner) (pop runner))) )
                (unless (null runner)
                  (error "Invalid keyword specifier ~S" token) )
                (add-keyword spec initform svar) ) )
            (t
              (error "Bad keyword specifier ~S" token) )) )

        ((:optional)
          (cond
            ((eq token' &aux)
              (setq state :aux) )
            ((eq token '&key)
              (setq key '&key)
              (setq state :key) )
            ((eq token '&rest)
              (setq state :rest) )
            ((symbolp token)
               (push (list token) opts) )
            ((consp token)
              (let* ((runner    token)
                     (ovar     (pop runner))
                     (initform (when (consp runner) (pop runner)))
                     (svar     (when (consp runner) (pop runner))) )
                  (unless (null runner)
                    (error "Invalid optional specifier ~S" token) )
                  (push (list ovar initform svar) opts) ) )
            (t
              (error "Bad optinal paramter ~S" token) )) )

        ((:required)
          (cond
            ((eq token '&aux)
              (setq state :aux) )
            ((eq token '&optional)
              (setq state :optional) )
            ((eq token '&rest)
              (setq state :rest) )
            ((eq token '&key)
              (setq key '&key)
              (setq state :key) )
            ((symbolp token)
               (push token reqs) )
            (t
               (error "Bad parameter name ~S" token) )) )

        ((:rest)
          (cond
            ((symbolp token)
              (setq rest token)
              (setq state :rest2) )
            (t
              (error "Bad parameter name ~S" token) )) )

        ((:rest2)
          (cond
            ((eq token '&aux)
              (setq state :aux) )
            ((eq token '&key)
              (setq key '&key)
              (setq state :key) )
            (t
              ;; Maybe missing rest parameter name.
              (error "Extra token after &rest ~S: ~S"
                rest token ) )) )) ) ) ) )
