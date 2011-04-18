;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Regex
;;; genesis/genesis-regex.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis-regex.lisp#1 $
;;;
;
(in-package #:si)

;; TODO yosi@msn.com 2008-06-21 We should have function for reading
;; captures slot of regex-match object.

;;; regex
(defun matched-p (match)
    (declare (values boolean))
    (declare (type regex-match match))
  (slot-value (the regex-match match) 'matched-p) )

;; Note: group can be null, e.g. capture in alternative (foo)|(bar).
(defun match-group (match nth)
    (declare (values (or range null)))
    (declare (type regex-match match))
    (declare (type sequence-index nth))
  (let ((v (slot-value (the regex-match match) 'captures)))
    (svref v nth) ) )

(defun match-group-count (match)
    (declare (values sequence-index))
    (declare (type regex-match match))
  (let ((v (slot-value (the regex-match match) 'captures)))
    (length v) ) )

(defun match-string (match &optional (nth 0))
    (declare (values (or string null)))
    (declare (type regex-match match))
  (when (matched-p match)
    (let ((group (match-group match nth)))
      (and group (range-string group)) )) )

;;; Test tool
(defun backslash (string)
    (declare (values simple-string))
    (declare (type string string))
  (with-input-from-string (in string)
  (with-output-to-string (out)
    (let ((acc 0)
          (count 0)
          (state :normal) )
      (loop
        (let ((ch (read-char in nil)))
          (unless ch (return))
          (ecase state
            ((:normal)
              (if (char= ch #\\)
                  (setq state :backslash)
                (write-char ch out) ) )
            ((:backslash)
              (case ch
                ((#\n)
                  (write-char #\Newline out)
                  (setq state :normal) )
                ((#\t)
                  (write-char #\Tab out)
                  (setq state :normal) )
                ((#\u)
                  (setq acc 0)
                  (setq count 0)
                  (setq state :unicode) )
                (otherwise
                  (write-char #\\ out)
                  (write-char ch out)
                  (setq state :normal) )) )
            ((:unicode)
              (let ((digit (digit-char-p ch 16)))
                (unless digit (error "Bad \\u"))
                (setq acc (logior (ash acc 4) digit))
                (incf count)
                (when (eql count 4)
                  (write-char (code-char acc) out)
                  (setq state :normal) ) ) )) )) ) ) ) )

(defun test-case (id pat txt opts expect)
    (declare (type simple-string id))
    (declare (type string pat))
    (declare (type list opts))
    (declare (type list expect))
    (declare (values t))
  (labels (
    (collect (match)
        (declare (values list))
        (declare (type regex-match match))
      (when (matched-p match)
        (let ((results '()))
          (dotimes (nth (match-group-count match) (nreverse results))
            (push (match-string match nth) results) ) )) )
    )
  (let* ((re (apply #'compile-regex pat opts))
         (m  (string-match re txt))
         (result (collect m)) )
    (let ((succeeded (equal expect result)))
      (if succeeded
          (format t "SUCCEEDED ~S~%" id)
        (format t "FAILED ~S expect=~S result=~S~%" id expect result) )

      (unless succeeded (error "FAILED"))
      succeeded ) ) ) )

