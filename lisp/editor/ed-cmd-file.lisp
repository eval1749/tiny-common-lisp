;;;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; Editor - Command - Files
;;; lisp/ed-cm-file.lisp
;;;
;;; This file is part of Evita.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/editor/ed-cmd-file.lisp#1 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(in-package :editor)

(defcommand file-drop ()
  (activate-buffer (load-file-buffer *event*)) )
