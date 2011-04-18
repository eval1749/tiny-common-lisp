;;;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; Editor - Runtime - Definitions
;;; lisp/ed-rtl-defs.lisp
;;;
;;; This file is part of Evita.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g00-loadup.lisp#18 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(in-package :editor)

(declaim (ftype (function (string) (or buffer null)) find-buffer))
