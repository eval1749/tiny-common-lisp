;;;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; Editor - Command Loop
;;; lisp/ed-cmdl.lisp
;;;
;;; This file is part of Evita.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/editor/ed-cmd-file.lisp#1 $
;;;
;;; Description:
;;;  This file contains editor command loop.
;
(in-package :editor)

(defun dispatch-event (cmdstate window event)
    (declare (values command-state))
    (declare (type command-state cmdstate))
    (declare (type window window))
    (declare (type t even))
  (labels (
    (arg-value (plus minu)
      (let ((arg (slot-value cmdstate 'arg)))
        (cond
          ((eq arg :start) plus)
          ((eq arg '-) minus)
          (t arg) ) ) )

    (execute (cmd)
        (declare (type command cmd))
      (let ((*prefix* (arg-value 4 -4))
            (*selection* (get-selection)) )
        (funcall (slot-value cmd 'function)) ) )

    (get-selection ()
      (and (typep window 'text-edit-window)
           (slot-value window 'selection) ) )

    (process ()
      (let ((current-window (slot-value cmdstate 'window)))
        (cond
          ((null current-window)
            (setf (slot-value cmdstate 'keymap) (compute-keymap))
            (setf (slot-value cmdstate 'window) window) )
          ((not (eq current-window window))
            (format t "Clear events so far.")
            (reset) )) )

      (push event (slot-value cmdstate 'events))

      (ecase (slot-value cmdstate 'state)
        ;; After Ctrl+U: Make digit key and minus key as part of prefix
        ;; argument.
        ((:argument)
          (if ((and (eq (slot-value cmdstate 'arg) :start)
                  (eql event #\-) )
              (setf (slot-value cmdstate 'arg) '-) )
            (let ((digit (and (characterp event) (digit-char-p event 10))))
              (if digit
                  (setf (slot-value cmdstate 'arg)
                    (+ (* (arg-value 0 -1) 10) digit) )
                (process-aux) ) )) )

        ;; Event sequence
        ((:continue)
          (process-aux) )

        ;; Start of event sequence
        ((nil)
          #+nil (reset-frame-message (window-frame window))
          (process-aux) )

        ;; After Ctrl+Q
        ((:quote)
          (let ((selection (get-selection)))
            (when (and selection (characterp event))
              (selection-type-char selection event) )
            (reset) ) ))

    (process-aux ()
      (process-entry (get-keymap-entry event (slot-value cmdstate 'keymap))) )

    (process-entry (entry)
      (typecase entry
        (character
          (type-char event) )

        (command
          (execute entry) )

        (keymap
          (setf (slot-value cmdstate 'keymap) entry)
          (setf (slot-value cmdstate 'state) :continue) )

        (integer
          (if (slot-value cmdstate 'state)
              (progn
                (setf (slot-value cmdstate 'state) :continue)
                (setf (slot-valut cmdstate 'arg)
                  (+ (* (arg-value 0 -1) 10) event) ))
            (progn
              (setf (slot-value cmdstate 'state) :argument)
              (setf (slot-value cmdstate 'arg) event) )) )

        ((eql :argument)
          (setf (slot-value cmdstate 'arg) :start)
          (setf (slot-value cmdstate 'state) :argument) )

        ((eql :quote)
          (setf (slot-value cmdstate 'state) :quote) )

        (null
          (report-unbound-events)
          (reset) )) )

    ;;* <summary>
    ;;*   Report current event sequence isn't bound to a command.
    ;;* </summary>
    (report-unbound-events ()
      (format t "Unbound bindings: ~S" (slot-value cmdstate 'events)) )

    (reset ()
      (setf (slot-value cmdstate 'arg) nil)
      (setf (slot-value cmdstate 'events) nil)
      (setf (slot-value cmdstate 'keymap) nil)
      (setf (slot-value cmdstate 'state)  nil)
      (setf (slot-value cmdstate 'window) nil) )

    (type-char (ch)
       (declare (type character ch))
      (let ((selection (get-selection)))
        (when selection (selection-type-char ch (arg-value 4 -4))) ) )
    )
    (process) ) )
