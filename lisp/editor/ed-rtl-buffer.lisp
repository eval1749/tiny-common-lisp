;;;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; Editor - Runtime - Buffer Functions
;;; lisp/ed-rtl-buffer.lisp
;;;
;;; This file is part of Evita.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/editor/ed-rtl-buffer.lisp#3 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(in-package :editor)

(defun activate-buffer (buffer)
    (declare (type buffer buffer))
  (let ((windows (buffer-windows buffer)))
    (if (null windows)
        (add-window (active-frame) (make-text-window buffer))
      (let ((active (first windows)))
          (declare (type window active))
        (dolist (window (rest windows) (activate-window active))
          (when (< (slot-value active 'active-tick)
                   (slot-value window 'active-tick) )
            (setq active window) ) ) )) ) )

(defun find-buffer (name)
    (declare (values (or buffer null)))
    (declare (type string name))
  (dolist (buffer *buffers*)
    (when (string= (buffer-name buffer) name)
      (return buffer) ) ) )

(defun find-file-buffer (thing)
    (declare (values (or buffer null)))
    (declare (type ext:pathname-designator thing))
  (let ((pathname (pathname thing)))
    (dolist (buffer *buffers*)
      (when (equal (buffer-pathname buffer) pathname)
        (return buffer) ) ) ) )

(defun load-file-buffer (thing &optional (external-format :default))
    (declare (values buffer))
    (declare (type ext:pathname-designator thing))
  (labels (
    (load-aux ()
      (let* ((pathname (pathname thing))
             (buffer (make-buffer (file-namestring pathname))) )
        (start-load-file buffer pathname external-format) ) )
    )
    (or (find-file-buffer thing) (load-aux)) ) )

(defun make-buffer (name)
    (declare (values buffer))
    (declare (type string name))
  (labels (
    (compute-unique-name (name counter)
        (declare (values string))
        (declare (type string name))
        (declare (type fixnum counter))
      (let ((newname (format nil "~A#~D" name counter)))
        (if (find-buffer newname)
            (compute-unique-name name (1+ counter))
          newname ) ) )

    (ensure-buffer-name (name)
        (declare (values string))
        (declare (type string name))
      (if (find-buffer name) (compute-unique-name name 2) name) )

    (make (name)
      (let ((buffer (make-instance 'buffer)))
        (setf (slot-value buffer 'name) name)
        (realize-instance buffer) ) )
    )
    (let ((buffer (make (ensure-buffer-name name))))
      (push buffer *buffers*)
      buffer ) ) )

(defun make-range (buffer &optional (start 0) end)
    (declare (values range))
    (declare (type buffer buffer))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
  (let ((range (make-instance 'range)))
    (setf (slot-value range 'buffer) buffer)
    (setf (slot-value range 'end) (or end (buffer-length buffer)))
    (setf (slot-value range 'start) start)
    (realize-instance range)
    range ) )
