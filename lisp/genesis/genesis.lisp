;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Genesis - Toplevel
;;; genesis/genesis.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/genesis/genesis.lisp#19 $
;;;
;
(in-package #:si)

;;; For compiler
;;; Protect compiler from errors during macro expansion.
(defun c::call-macro-expander (expander form env)
  (handler-case
      (values (funcall *macroexpand-hook* expander form env) nil)
    (error (c) (values nil c)) ) )

;;; C-discriminator calls this function if method in method cache.
(defun compute-effective-method-using-arguments (gf args)
    (declare (type generic-function gf))
  (error "There is no applicable method for ~S with ~S." gf args) )

(defun cl:load (filename &key
                    (external-format :default)
                    (if-does-not-exist :error)
                    (print   *load-print*)
                    (verbose *load-verbose*) )
  (labels (
    (load-sexp (path in)
        (declare (type stream in))
      (let ((c::*situation*         'load)
            (*load-pathname*        path)
            (*load-print*           print)
            (*load-truename*        (truename path))
            (*load-verbose*         verbose)
            (*package*              *package*)
            (*readtable*            *readtable*)
            (*read-line-number-table*
                (and (stream-line-number in) (make-hash-table :test 'eq)) )
            (eof                    '(end-of-file)) )
        (let ((*default-pathname-defaults*
                (make-pathname
                    :directory (pathname-directory *load-truename*) )))
          (loop
            (let ((form (read in nil eof)))
              (when (eq form eof)
                (when *read-line-number-table*
                  (clrhash *read-line-number-table*) )
                (return t) )
              (let ((value (eval form)))
                (if *load-print* (format t "~S~%" value)) ) )) ) ) )
    )
  (let ((path (merge-pathnames filename (make-pathname :type "lisp"))))
    (let ((in (open path
                :external-format external-format
                :if-does-not-exist (and if-does-not-exist :error) ) ))
      (when in
        (when verbose
          (format t "; Loading ~S~%" path) )
        (unwind-protect
            (load-sexp path in)
          (close in)
          (when verbose
            (format t "; End of loading ~S~%" path) ))
       t ) ) ) ) )

(defun cl:proclaim (declspec)
    (declare (ignore declspec))
  nil )

#+nil
(defmacro cl:declaim (&rest declspec*)
  `(eval-when (:execute :compile-toplevel :load-toplevel)
    ,@(mapcar (lambda (x) `(proclaim ',x))) ) )

(defun cl:invoke-debugger (cond)
  (force-output)
  (force-output *error-output*)
  (let ((*standard-output* *error-output*))
    (format t ";;; Error: ~A~%" cond)
    (format t ";;; Backtrace~%")
    (let ((nth 0))
      (dolist (frame (si::make-stack-trace))
        (format t "; [~D] ~S~%" nth frame)
        (incf nth) ) )
    (force-output) )
  (si::%exit-lisp 1) )

(setq *load-print* t)
(load "../../lisp/genesis/genesis-macro.lisp")

(load "../../lisp/genesis/genesis-runtime.lisp")

(load "../../lisp/macro/m01-intro.lisp")
(load "../../lisp/macro/m05-control.lisp")
(load "../../lisp/macro/m05-data.lisp")
(load "../../lisp/macro/m06-do.lisp")
(load "../../lisp/macro/m09-cond.lisp")
(load "../../lisp/macro/m14-cons.lisp")
(load "../../lisp/macro/m21-stream.lisp")
(load "../../lisp/macro/m22-printer.lisp")

(load "/proj/evedit2/lisp/runtime/r01-intro.lisp")
(load "/proj/evedit2/lisp/runtime/r03-eval.lisp")
(load "/proj/evedit2/lisp/runtime/r04-type.lisp")
(load "/proj/evedit2/lisp/runtime/r05-control.lisp")
(load "/proj/evedit2/lisp/runtime/r05-data.lisp")
(load "/proj/evedit2/lisp/runtime/r07-object.lisp")
(load "/proj/evedit2/lisp/runtime/r09-cond.lisp")
(load "/proj/evedit2/lisp/runtime/r10-symbol.lisp")

(load "/proj/evedit2/lisp/runtime/r12-byte.lisp")
(load "/proj/evedit2/lisp/runtime/r12-integer.lisp")
(load "/proj/evedit2/lisp/runtime/r12-number.lisp")
(load "/proj/evedit2/lisp/runtime/r12-real.lisp")

(load "/proj/evedit2/lisp/runtime/r13-char.lisp")
(load "/proj/evedit2/lisp/runtime/r14-cons.lisp")
(load "/proj/evedit2/lisp/runtime/r14-set.lisp")
(load "/proj/evedit2/lisp/runtime/r16-string.lisp")
(load "/proj/evedit2/lisp/runtime/r17-list.lisp")
(load "/proj/evedit2/lisp/runtime/r17-sequence.lisp")
(load "/proj/evedit2/lisp/runtime/r17-vector.lisp")
(load "/proj/evedit2/lisp/runtime/r21-stream.lisp")
(load "/proj/evedit2/lisp/runtime/r22-printer.lisp")

(load "../../lisp/genesis/genesis-regex.lisp")

(load "../../lisp/genesis/genesis-clos.lisp")
(load "../../lisp/macro/m07-object.lisp")

;; From here we can use defmethod.
(defgeneric cl:class-name (o))
(defmethod cl:class-name ((o class)) (slot-value o 'name))

(load "/proj/evedit2/lisp/runtime/r22-print-object.lisp")

;;= <FIXME date="2008-12-27" by="yosi@msn.com">
;;=     Move below functions to other files..
(defun cl:make-string (size &key
                    (element-type 'character)
                    (initial-element #\Space initp) )
    (declare (values simple-string))
    (declare (type sequence-index size))
    (declare (type character initial-element))
  (unless (eq element-type 'character)
    (error "Element type must be character.") )
  (let* ((classd #.(slot-value (find-class 'simple-string)
                               'instance-description ))
         (str (allocate-binvec classd size)) )
    (when initp
      (dotimes (i size)
        (setf (schar str i) initial-element) ))
    str ) )

(defgeneric ext:stream-read-line (stream))

(defmethod ext:stream-read-line ((stream stream))
    (declare (values simple-string t))
  (labels (
    (copy (dst src k)
      (dotimes (i k dst) (setf (schar dst i) (schar src i))) )
    (get-line (line k)
       (copy (make-string k) line k) )
    )
  (let ((line (make-string 80))
        (k 0) )
    (loop
      (let ((ch (stream-read-char stream)))
        (when (eq ch :eof) (return (values (get-line line k) t)))
        (when (eql ch #\Newline) (return (values (get-line line k) nil)))
        (unless (< k (length line))
          (setq line (copy (make-string (+ k 80)) line k)) )
        (setf (schar line k) ch)
        (incf k) )) ) ) )
;;= </FIXME>

(defun cl:lisp-implementation-type ()
  (values "Tiny Common Lisp") )

(defun cl:lisp-implementation-version ()
  (values
    #.(with-open-file (s "/proj/evedit2/build.txt") (stream-read-line s)) ) )

(load "../../lisp/runtime/r60-dom-defs.lisp")
(load "../../lisp/runtime/r60-dom-fns.lisp")
(load "../../lisp/runtime/r60-dom-sexp.lisp")


#+editor (load "../../lisp/genesis/genesis-editor.lisp")

(load "../../lisp/devel/d03-eval.lisp")
(load "../../lisp/devel/d22-print-object.lisp")
#-editor (load "../../lisp/devel/d25-toplevel.lisp")

;;= <FIXME date="2009-01-18" by="yosi@msn.com">
;;=  We should use proper add-method to update method-cache.
(defun flush-method-cache (gf)
    (declare (values generic-function))
    (declare (type generic-function gf))
  (let ((mc (slot-value gf 'method-cache)))
    (fill (slot-value mc 'vector) 0)
    gf ) )

(flush-method-cache #'print-object)
;;= </FIXME>

(defun save-it ()
    (labels (
      (save (filename)
        (format t "; Save image to ~S~%" filename)
        (let ((*image-save-time* (multiple-value-list (get-decoded-time))))
          (save-image filename) ) )
      )
      (save (make-pathname
              :type "image"
              :defaults (car *command-line-arguments*) )) ) )

*features*

#-editor (collect-garbage)

(save-it)
