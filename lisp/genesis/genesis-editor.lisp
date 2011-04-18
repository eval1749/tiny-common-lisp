;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Editor
;;; genesis/genesis-editor.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis-editor.lisp#3 $
;;;
;
(in-package :editor)

;; FIXME 2008-08-02 yosi@msn.com We should put function value.

;;; Buffer readers
(macrolet (
  (define (slot-name)
   `(defun ,(intern (format nil "~A-~A" 'buffer slot-name)) (buffer)
      (slot-value (the buffer buffer) ',slot-name) ) )
  )
  (define external-format)
  (define length)
  (define mode)
  (define name)
  (define pathname)
  (define read-only)
  (define windows)
 ) ; macrolet

;;; Range readers
(macrolet (
  (define (slot-name)
   `(defun ,(intern (format nil "~A-~A" 'range slot-name)) (range)
      (slot-value (the range range) ',slot-name) ) )
  )
  (define buffer)
  (define end)
  (define start)
 ) ; macrolet

;;; Selection readers
(macrolet (
  (define (slot-name)
   `(defun ,(intern (format nil "~A-~A" 'selection slot-name)) (selection)
      (slot-value (the selection selection) ',slot-name) ) )
  )
  (define active)
  (define goal-x)
  (define goal-y)
  (define line-number)
  (define window)
 ) ; macrolet

;;; Text Window readers
(macrolet (
  (define (slot-name)
   `(defun ,(intern (format nil "~A-~A" 'text-window slot-name)) (window)
      (slot-value (the text-window window) ',slot-name) ) )
  )
  (define selection)
 ) ; macrolet

;;; <summary>
;;;   Expansion of DEFCOMMAND macro.
;;; </summary>
(defun %defcommand (name fn plist)
    (declare (values symbol))
    (declare (type symbol name))
    (declare (type function fn))
    (declare (type list plist))
  (unless (and name (symbolp name))
    (error "Command name must be a non-NIL symbol: ~S" name) )
  (setf (gethash name *command-table*)
    (make-instance 'command :name name :function fn :plist plist) )
  name )

(defmacro ed:defcommand (name (&rest plist) &body body)
 `(eval-when (:compile-toplevel :execute :load-toplevel)
    (%defcommand ',name
      (lambda ()
          (declare (ext:function-name (command ,name)))
          (declare (values t))
         ,@body )
      ',plist ) ) )

(load "../editor/ed-rtl-buffer")
(load "../editor/ed-rtl-window")
(load "../editor/ed-cmd-file")

(defun install-global-binding (event-type command-name)
  (setf (gethash event-type (slot-value *global-keymap* 'bindings))
        (gethash command-name *command-table*) ) )

(install-global-binding (type-of #p"foo") 'file-drop)
