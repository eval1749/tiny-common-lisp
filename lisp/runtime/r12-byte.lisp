;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 12 Numbers - Bytes
;;; runtime/r12-byte.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r12-byte.lisp#2 $
;;;
;
(in-package #:si)

;;; [B]

(defun cl:byte (size position)
    (declare (values byte-specifier))
    (declare (type unsigned-byte position size))
  (make-instance 'byte-specifier :position position :size size) )

;;; [D]

(defun cl:deposit-field (newbyte bytespec integer)
  (let ((mask (ash (ldb (byte (byte-size bytespec) 0) -1)
                   (byte-position bytespec) )) )
    (logior (logand newbyte mask)
            (logand integer (lognot mask)) ) ) )

(defun cl:dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec)))))
    (logior (logand integer (lognot (ash mask (byte-position bytespec))))
            (ash (logand newbyte mask) (byte-position bytespec)) ) ) )

;;; [L]

(defun cl:ldb (bytespec integer)
    (declare (values integer))
    (declare (type byte-specifier bytespec))
    (declare (type integer integer))
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec))) ) )

(defun cl:ldb-test (bytespec integer)
    (declare (type byte-specifier bytespec))
    (declare (type integer integer))
  (not (zerop (ldb bytespec integer))) )

;;; [M]

(defun cl:mask-field (bytespec integer)
    (declare (type byte-specifier bytespec))
  (logand integer (dpb -1 bytespec 0)) )
