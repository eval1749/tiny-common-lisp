;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 16 Strings
;;; runtime/r16-string.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r16-string.lisp#1 $
;;;
;
(in-package #:si)

;;; [C]
(defun cl:char (string index)
    (declare (values character))
    (declare (type string string))
    (declare (type sequence-index index))
  (multiple-value-bind (data index) (string-data string index index)
    (schar data index) ) )

(defun (setf cl:char) (ch string index)
    (declare (values character))
    (declare (type character ch))
    (declare (type string string))
    (declare (type sequence-index index))
  (multiple-value-bind (data index) (string-data string index index)
    (setf (schar data index) ch) ) )

;;; [S]
(defun string-data (string &optional (start 0) end)
    (declare (values simple-string sequence-index sequence-index))
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
  (labels (
    (follow (runner offset)
        (declare (values simple-string sequence-index))
        (declare (type string runner))
        (declare (type sequence-index offset))
      (typecase runner
        (simple-string
          (values runner offset) )
        (string-object
          (follow (slot-value runner 'displaced-to)
                  (+ (slot-value runner 'offset) offset) ) )
        (otherwise
          (error "Broken vector ~S." runner) )) )
    )
    (multiple-value-bind (data offset length)
        (typecase string
          (simple-string
            (values string 0 (slot-value string 'length)) )
          (string-object
            (multiple-value-bind (data offset)
                (follow (slot-value string 'displaced-to)
                        (slot-value string 'offset) )
              (values data offset (slot-value string 'fill-pointer)) ) )
          (otherwise
            (error 'type-error :datum string :expected-type 'string) ))
      (let ((end (or end length)))
        (unless (<= 0 start end length)
          (bounding-index-error string start end) )
        (values data (+ start offset) (+ end offset)) ) ) ) )
