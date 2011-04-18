;;;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; Editor - Runtime - Windowing Functions
;;; lisp/ed-rtl-window.lisp
;;;
;;; This file is part of Evita.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/editor/ed-rtl-window.lisp#2 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(in-package :editor)

(defun active-frame ()
    (declare (values (or frame null)))
  (when *frames*
    (let ((active (first *frames*)))
        (declare (type frame active))
      (dolist (frame (rest *frames*) active)
        (when (< (slot-value active 'active-tick)
                 (slot-value frame  'active-tick) )
           (setq active frame) ) ) )) )

(defun make-text-window (buffer)
    (declare (values text-window))
  (labels (
    (make-selection (window buffer)
      (let ((selection (make-instance 'selection)))
        (setf (slot-value selection 'active) :start)
        (setf (slot-value selection 'buffer) buffer)
        (setf (slot-value selection 'end) 0)
        (setf (slot-value selection 'goal-x) -1)
        (setf (slot-value selection 'goal-y) -1)
        (setf (slot-value selection 'line-number) 0)
        (setf (slot-value selection 'start) 0)
        (setf (slot-value selection 'window) window)
        (realize-instance selection)
        selection ) )
    )
    (let ((window (make-instance 'text-window)))
      (setf (slot-value window 'active-tick) 0)
      (setf (slot-value window 'blink) (make-range buffer))
      (setf (slot-value window 'parent) nil)
      (setf (slot-value window 'range) (make-range buffer))
      (setf (slot-value window 'peer) 0)
      (setf (slot-value window 'selection) (make-selection window buffer))
      window ) ) )
