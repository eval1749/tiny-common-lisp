;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Command Loop
;;; genesis/genesis-cmdl.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/devel/d25-toplevel.lisp#4 $
;;;
;
(in-package #:devel)

(defun command-loop (cond)
  (labels (
    (command/backtrace ()
      (let ((k 0))
        (dolist (stack (si::make-stack-trace))
          (format t "; ~D ~S~%" k stack)
          (incf k) ) ) )

    (command/condition ()
      (if *condition*
          (remember-values (list *condition*) :print)
        (format t "; No condition is signaled so far.~%") ) )

    (command/continue ()
      (invoke-restart (nth (get-arg "Number") (compute-restarts))) )

    (command/macroexpand-1 ()
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 (get-arg "Form"))
        (if expanded-p
            (remember-values (list expansion) :print)
          (format t "; No expansion~") ) ) )

    (command/restarts ()
      (let ((nth 0))
        (dolist (restart (compute-restarts))
          (format t "; [~D] ~A~%" nth restart)
          (incf nth) ) ) )

    (command/toplevel ()
      (invoke-restart (first (last (compute-restarts)))) )

    (command-loop-aux (cond)
      (let ((*standard-input* *terminal-io*)
            (*standard-output* *terminal-io*)
            (*cmdl-level* (1+ *cmdl-level*))
            (*condition* cond) )
        (when cond
          (format t "~%; Error: ~A~%" cond)
          (force-output)
          (remember-values (list cond)) )
        (let ((eof '(:eof)))
          (loop
            (prompt)
            (let ((form (read nil nil eof)))
              (when (eq form eof) (return))
              (unless (execute-command form)
                (setq - form)
                (let ((vals (multiple-value-list (eval form))))
                  (shiftf +++ ++ +  -)
                  (remember-values vals :print) ) )) ) ) ) )

    (execute-command (form)
      (typecase form
        (cons
          (let ((name (first form)))
            (cond
              ((and (consp name) (eq (first name) 'lambda))
                #+nil "lambda expression"
                nil )
              ((not (symbolp name))
                (format t "; Invalid form~%")
                :invalid )
              ((fboundp name)
                nil )
              (t
                (format t "; Function ~S is undefined.~%" name)
                :undefined )) ) )
        (symbol
          (let ((name (intern (symbol-name form) :keyword)))
            (case name
              ((:bt)      (command/backtrace) t)
              ((:cond)    (command/condition) t)
              ((:cont)    (command/continue) t)
              ((:exit)    (exit-lisp 0))
              ((:des)     (describe *) t)
              ((:dis)     (disassemble (get-arg "Function")) t)
              ((:ld)      (load (get-arg "Filename")) t)
              ((:m1)      (command/macroexpand-1) t)
              ((:rs)      (command/restarts) t)
              ((:top)     (command/toplevel) t)
              (otherwise
                (unless (boundp form)
                  (format t "; Variable ~S is unbound.~%" form)
                  :unbound ) )) ) )) )

    (get-arg (name)
        (declare (ignore name))
      (read) )

    (prompt ()
      (dotimes (k *cmdl-level*)
        (write-char #\>) )
      (write-char #\Space)
      (force-output) )

    (remember-values (vals &optional printp)
      (if (null vals)
          (when printp (format t "; No values~%"))
        (progn
          (shiftf *** ** * (first vals))
          (shiftf /// // / vals)
          (cond
            ((not printp))
            ((null (rest vals))
              (let ((val (first vals)))
                (format t "; ~S: ~S~%" (type-of val) val) ) )
            (t
              (let ((k 0))
                (dolist (val vals)
                  (format t "; [~D] ~S: ~S~%"
                    k
                    (type-of val)
                    val )
                  (incf k) ) ) )))) )
    )
    (let ((level *cmdl-level*))
      (loop
        (with-simple-restart (abort (if (zerop level)
                                      "Return to toplevel"
                                    "Return to comand level ~D" )
                                  level )
          (command-loop-aux cond) )
        (format t "~%; Back to command level ~D~%" level) )) ) )

(defun ext:exit-lisp (code)
    (declare (type (unsigned-byte 32) code))
  #+nil "run scheduled-finalization"
  (si::%exit-lisp code) )

(defun cl:invoke-debugger (cond)
  (when *debugger-hook*
    (let ((hook *debugger-hook*) (*debugger-hook* nil))
      (funcall hook) ))
  (if (interactive-stream-p *standard-input*)
      (progn
        (command-loop cond)
        (exit-lisp 1) )
    (progn
      (force-output)
      (force-output *error-output*)
      (format t ";;; Error: ~A~%" cond)
      (format t ";;; Backtrace:~%")
      (let ((nth 0))
        (dolist (frame (si::make-stack-trace))
          (format t ";; ~D ~S~%" nth frame)
          (incf nth) )))) )

(defun si::start-application ()
  (labels (
    (batch-loop ()
      (let ((eof '(:eof)))
        (loop
          (let ((form (read nil nil eof)))
            (when (eq form eof) (return))
            (eval form) )) ) )

    (herald ()
      (multiple-value-bind (s n h d m y)
          (values-list si::*image-save-time*)
        (format t ";;; ~A ~A~%"
            (lisp-implementation-type)
            (lisp-implementation-version) )
        (format t ";;; Copyright (C) 2007-2008 by Project Vogue.~%")
        (format t ";;; Image time: ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0:D:~2,'0:D~%"
            y m d
            h n s )
        (format t ";;; Features: ~S~%" *features*)
        (format t ";~%") ) )

    (toplevel ()
      (let ((*cmdl-level* 0))
        (herald)
        (command-loop nil) ) )
    )
  (if (interactive-stream-p *standard-input*)
      (toplevel)
    (batch-loop) ) ) )
