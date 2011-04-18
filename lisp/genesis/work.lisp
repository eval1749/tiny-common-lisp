(in-package :c)
;(setq c::*options* '((debug . compile)))
;(defun foo () (multiple-value-bind (a b c d) (bar) (values a b c d)))

(setq doc (dom:make-document "foo"))
(setf (dom:attribute (dom:document-element doc) "x") "abcdef")
(dom:serialize doc *standard-output*)

(defun foo (x)
    (declare (type dom:parent-node x))
  (slot-value x 'last-child) )

(defun childs (x)
  (si::with-collector (collect child-nodes)
    (let ((runner (slot-value x 'dom:first-child)))
      (loop
        (unless runner (return (child-nodes)))
        (collect runner)
        (setq runner (slot-value runner 'dom:next-sibling)) ) )) )
