;;;; -*- Mode: Lisp; -*-
;;; TinyCl - Runtime - 60 Document Object Model
;;; runtime/r60-dom-fns.lisp
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evedit2/mainline/lisp/runtime/r60-dom-fns.lisp#4 $
;;;
;
(in-package :dom)

;;; [%]
(defun %append-child (parent-node new-child)
    (declare (values dom:node))
    (declare (type dom:parent-node parent-node))
    (declare (type dom:node new-child))
  (let ((last-child (slot-value parent-node 'last-child)))
    (if last-child
        (setf (slot-value last-child 'next-sibling) new-child)
      (setf (slot-value parent-node 'first-child) new-child) )
    (setf (slot-value parent-node 'last-child) new-child)
    (setf (slot-value new-child 'next-sibling) nil)
    (setf (slot-value new-child 'previous-sibling) last-child)
    (setf (slot-value new-child 'parent-node) parent-node)
    new-child ) )

(defun %insert-after (parent-node new-child ref-child)
    (declare (values dom:node))
    (declare (type dom:parent-node parent-node))
    (declare (type dom:node new-child ref-child))
  (let ((next (slot-value ref-child 'previous-next)))
    (if next
        (setf (slot-value next 'previous-sibling) new-child)
      (setf (slot-value parent-node 'last-child) new-child) )
    (setf (slot-value new-child 'next-sibling) next)
    (setf (slot-value new-child 'previous-sibling) ref-child)
    (setf (slot-value ref-child 'next-sibling) new-child) ) )

(defun %insert-before (parent-node new-child ref-child)
    (declare (values dom:node))
    (declare (type dom:parent-node parent-node))
    (declare (type dom:node new-child ref-child))
  (let ((prev (slot-value ref-child 'previous-sibling)))
    (if prev
        (setf (slot-value prev 'next-sibling) new-child)
      (setf (slot-value parent-node 'first-child) new-child) )
    (setf (slot-value new-child 'next-sibling) ref-child)
    (setf (slot-value new-child 'previous-sibling) prev)
    (setf (slot-value ref-child 'previous-sibling) new-child) ) )

(defun %prepend-child (parent-node new-child)
    (declare (values dom:node))
    (declare (type dom:parent-node parent-node))
    (declare (type dom:node new-child))
  (let ((first-child (slot-value parent-node 'first-child)))
    (if first-child
        (setf (slot-value first-child 'previous-sibling) new-child)
      (setf (slot-value parent-node 'last-child) new-child) )
    (setf (slot-value parent-node 'first-child) new-child)
    (setf (slot-value new-child 'next-sibling) first-child)
    (setf (slot-value new-child 'previous-sibling) nil)
    (setf (slot-value new-child 'parent-node) parent-node)
    new-child ) )

(defun %remove-child (parent-node old-child)
    (declare (values dom:node))
    (declare (type dom:parent-node parent-node))
    (declare (type dom:node old-child))
  (let ((next (slot-value old-child 'next-sibling))
        (prev (slot-value old-child 'previous-sibling)) )
    (if next
        (setf (slot-value next 'previous-sibling) prev)
      (setf (slot-value parent-node 'last-child) prev) )
    (if prev
        (setf (slot-value prev 'next-sibling) next)
      (setf (slot-value parent-node 'first-child) next) ) )
    old-child )

;;; [A]
(defmethod dom:append-child ((doc dom:document) new-child)
  (unless (eq (owner-document new-child) doc)
    (error 'dom:wrong-document :document doc :node new-child) )
  (typecase new-child
    (dom:comment
      (%remove-child (slot-value new-child 'parent-node) new-child)
      (%append-child doc new-child) )
    (dom:element
      (when (dom:document-element doc)
        (error 'dom:hierachy-error :parent-node doc :node new-child) )
      (%remove-child (slot-value new-child 'parent-node) new-child)
      (%append-child doc new-child) )
    (otherwise
        (error 'dom:hierachy-error :parent-node doc :node new-child) )) )

(defmethod dom:append-child ((parent-node dom:parent-node) new-child)
  (unless (eq (owner-document new-child) (owner-document parent-node))
    (error 'dom:wrong-document :document parent-node :node new-child) )
  (typecase new-child
    ((or dom:character-data dom:element)
      (%remove-child (slot-value new-child 'parent-node) new-child)
      (%append-child parent-node new-child) )
    (dom:document-fragment
      (loop
        (let ((node (slot-value new-child 'first-child)))
          (unless node (return new-child))
          (%remove-child new-child node)
          (%append-child parent-node node) )) )
    (otherwise
        (error 'dom:hierachy-error :parent-node parent-node :node new-child) )) )

(defmethod dom:append-child ((node dom:node) new-child)
  (error 'dom:hierachy-error :parent-node node :node new-child) )

(defmethod dom:attribute ((node dom:node) qname &optional namespace-uri)
    (declare (values (or string null)))
    (declare (type string qname))
    (declare (type (or string null) namespace-uri))
  (let ((attr (dom:attribute-node node qname namespace-uri)))
    (when attr (dom:node-value attr)) ) )

(defmethod (setf dom:attribute)
    (nv (elt dom:element) qname &optional ns-uri)
  (let ((attr
          (or (dom:attribute-node elt qname ns-uri)
            (let ((attr (make-instance 'dom:attribute
                            :name (parse-qname qname ns-uri) )))
              (%prepend-child elt attr) )) ))
    (setf (dom:node-value attr) nv) ) )

(defmethod (setf dom:attribute)
                (nv (node dom:node) qname &optional ns-uri)
    (declare (ignore nv qname ns-uri))
  (error 'no-modification-allowed :node node) )

(defmethod dom:attribute-node
    ((elt dom:element) qname &optional namespace-uri)
  (let* ((nameobj (parse-qname qname namespace-uri))
         (xname (slot-value nameobj 'expanded-name))
         (runner (slot-value elt 'first-child)) )
    (loop
      (unless (typep runner 'dom:attribute)
        (return nil) )
      (let ((present (slot-value runner 'name)))
        (when (or (eq present nameobj)
                  (eq (slot-value present 'expanded-name) xname) )
          (return runner) ) )
      (setq runner (slot-value runner 'next-sibling)) ) ) )

(defmethod dom:attribute-node ((node dom:node))
  nil )

;;; [D]
(defun dom:document-element (doc)
    (declare (values (or dom:element null)))
    (declare (type dom:document doc))
  (let ((runner (slot-value doc 'first-child)))
    (loop
      (when (null runner) (return nil))
      (when (typep runner 'dom:element) (return runner))
      (setq runner (slot-value runner 'next-sibling)) ) ) )

;;; [F]
(defmethod dom:first-child ((elt dom:parent-node))
  (let ((runner (slot-value elt 'first-child)))
    (loop
      (unless (typep runner 'dom:attribute) (return runner))
      (setq runner (slot-value runner 'next-sibling)) ) ) )

(defmethod dom:first-child ((node dom:node))
  nil )

(defmethod dom:first-child ((parent-node dom:parent-node))
  (slot-value parent-node 'first-child) )

;;; [I]

;; An expanded name is a pair consisting of namespace name and local name.
(defun intern-expanded-name (ns local-name)
    (declare (values expanded-name))
    (declare (type dom:namespace ns))
    (declare (type string local-name))
  (let ((htb (slot-value ns 'expanded-table)))
    (or (values (gethash local-name htb))
        (let ((name (make-instance 'expanded-name)))
          (setf (slot-value name 'local-name) local-name)
          (setf (slot-value name 'namespace)  ns)
          (setf (gethash name htb) name) )) ) )

(defun intern-namespace (namespace-uri)
    (declare (values dom:namespace))
  (or (values (gethash namespace-uri *namespace-table*))
      (let ((ns (make-instance 'dom:namespace)))
        (setf (slot-value ns 'uri) namespace-uri)
        (setf (slot-value ns 'expanded-table)
            (make-hash-table :test 'equal) )
        (setf (slot-value ns 'qualified-table)
            (make-hash-table :test 'equal) )
        ns )) )

(defun intern-name-object (xname qname)
    (declare (values name-object))
    (declare (type expanded-name xname))
    (declare (type string qname))
  (let* ((ns  (slot-value xname 'namespace))
         (htb (slot-value ns 'qualified-table)) )
    (or (values (gethash qname htb))
        (let ((nameobj (make-instance 'name-object)))
          (setf (slot-value nameobj 'qualified-name) qname)
          (setf (slot-value nameobj 'expanded-name) xname)
          (setf (gethash qname htb) nameobj) )) ) )

;;; [I]
(defmethod dom:insert-after
      ((parent-node dom:parent-node) new-child ref-child)
  (unless (eq (owner-document new-child) (owner-document parent-node))
    (error 'dom:wrong-document :document parent-node :node new-child) )
  (typecase new-child
    ((or dom:character-data dom:character-data)
      (if ref-child
          (%insert-after parent-node new-child ref-child)
        (%prepend-child parent-node new-child) ) )
    (dom:document-fragment
      (unless ref-child
        (let ((node (slot-value new-child 'first-child)))
          (when node
            (%remove-child new-child node)
            (setq ref-child (%prepend-child parent-node node))) ))
      (loop
        (let ((node (slot-value new-child 'first-child)))
          (unless node (return new-child))
          (%remove-child new-child node)
          (setq ref-child (%insert-after node ref-child)) )) )
    (otherwise
      (error 'dom:hierachy-error
        :node new-child
        :parent-node parent-node ) )) )

(defmethod dom:insert-after ((node dom:node) new-child ref-child)
    (declare (ignore ref-child))
  (error 'dom:hierachy-error :parent-node node :node new-child) )

(defmethod dom:insert-before
      ((parent-node dom:parent-node) new-child ref-child)
  (unless (eq (owner-document new-child) (owner-document parent-node))
    (error 'dom:wrong-document :document parent-node :node new-child) )
  (typecase new-child
    ((or dom:character-data dom:character-data)
      (%remove-child (slot-value new-child 'parent-node) new-child)
      (if ref-child
          (%insert-before parent-node new-child ref-child)
        (%append-child parent-node new-child) ) )
    (dom:document-fragment
      (if ref-child
          (loop
            (let ((node (slot-value new-child 'first-child)))
              (unless node (return new-child))
              (%remove-child new-child node)
              (%append-child parent-node node) ))
        (loop
          (let ((node (slot-value new-child 'first-child)))
            (unless node (return new-child))
            (%remove-child new-child node)
            (setq ref-child (%insert-before node ref-child)) ))) )
    (otherwise
        (error 'dom:hierachy-error
            :node new-child
            :parent-node parent-node ) )) )

(defmethod dom:insert-before ((node dom:node) new-child ref-child)
    (declare (ignore ref-child))
  (error 'dom:hierachy-error :parent-node node :node new-child) )

;;; [L]
(defmethod dom:last-child ((elt dom:parent-node))
  (let ((node (slot-value elt 'last-child)))
    (and (not (typep node 'dom:attribute)) node) ) )

(defmethod dom:last-child ((node dom:node))
  nil )

;;; [M]
(defun dom:make-attribute (doc qname &optional namespace-uri)
  (let ((attr (make-instance 'dom:attribute)))
    (setf (slot-value attr 'name) (parse-qname qname namespace-uri))
    (%append-child (slot-value doc 'floating-node) attr)
    attr ) )

;;; Correspond to
;;;   DOMImplementation.createDocument
(defun dom:make-document (qname &optional namespace-uri)
    (declare (values dom:document))
    (declare (type string qname))
    (declare (type (or string null) namespace-uri))
  (labels (
    (install-float-node (doc)
      (let ((float (make-instance 'floating-node :parent-node doc)))
       (setf (slot-value doc 'floating-node) float)
       (setf (slot-value doc 'parent-node) float)
       (setf (slot-value float 'parent-node) doc) ) )
    )
    (let ((doc (make-instance 'dom:document)))
      (install-float-node doc)
      (append-child doc (dom:make-element doc qname namespace-uri))
      doc ) ) )

(defun dom:make-element (doc qname &optional namespace-uri)
    (declare (values dom:element))
    (declare (type dom:document doc))
    (declare (type string qname))
    (declare (type (or string null) namespace-uri))
  (let ((elt (make-instance 'dom:element)))
    (setf (slot-value elt 'name) (parse-qname qname namespace-uri))
    (%append-child (slot-value doc 'floating-node) elt) ) )

(defun dom:make-text (doc data)
    (declare (values dom:text))
    (declare (type dom:document doc))
    (declare (type string-designator data))
  (let ((text (make-instance 'dom:text :data (string data))))
    (%append-child (slot-value doc 'floating-node) text)
    text ) )

;;; [N]
(defmethod dom:next-sibling ((node dom:node))
  (slot-value node 'next-sibling) )

(defmethod dom:node-name ((cs dom:cdata-section))
  "#cdata-section" )

(defmethod dom:node-name ((comment dom:comment))
  "#comment" )

(defmethod dom:node-name ((doc dom:document))
  "#document" )

(defmethod dom:node-name ((df dom:document-fragment))
  "#document-fragment" )

(defmethod dom:node-name ((node named-node))
  (slot-value (slot-value node 'name) 'qualified-name) )

(defmethod dom:node-name ((doc dom:text))
  "#text" )

(defmethod dom:node-value ((attr dom:attribute))
  (with-output-to-string (sout) (text-content attr sout)) )

(defmethod dom:node-value ((node dom:character-data))
  (slot-value node 'data) )

(defmethod dom:node-value ((node dom:node))
  nil )

(defmethod (setf dom:node-value) (new-value (attr dom:attribute))
  (loop
    (let ((node (slot-value attr 'first-child)))
      (unless node (return))
      (%remove-child attr node) ))
  (let ((text (make-instance 'dom:text :data new-value)))
    (%append-child attr text)
    new-value ) )

(defmethod (setf dom:node-value) (new-value (cd dom:character-data))
  (setf (slot-value cd 'data) (string new-value)) )

(defmethod (setf dom:node-value) (new-value (node dom:node))
    (declare (ignore new-value))
  (error 'no-modification-allowed :node node) )

;;; [O]
(defmethod dom:owner-document ((node dom:node))
    (declare (values dom:document))
  (let ((runner node))
    (loop
      (when (typep runner 'dom:document) (return runner))
      (setq runner (slot-value runner 'parent-node)) ) ) )

;;; [P]
(defmethod dom:parent-node ((node dom:node))
  (let ((parent-node (slot-value node 'parent-node)))
    (and (not (typep parent-node 'floating-node)) parent-node) ) )

;; [7] QName ::= PrefixedName | UnprefixedName
;; [8] PrefixedName ::= Prefix ':' LocalPart
;; [9] UnprefixedName ::= LocalPart
;; [10] Prefix ::= NCName
;; [11] LocalPart ::= NCName
(defun parse-qname (qname namespace-uri)
    (declare (values name-object))
    (declare (type string qname))
    (declare (type (or string null namespace-uri)))
  (let* ((namespace
           (if namespace-uri
              (intern-namespace namespace-uri)
            +unspecified-namespace+ ) )
         (colon (position #\: qname))
         (local-name (if colon (subseq qname (1+ colon)) qname)) )
    (when (and colon (null namespace-uri))
      (error "Can't have prefix for unspecified namespace.") )
    (let ((xname (intern-expanded-name namespace local-name)))
      (intern-name-object xname qname) ) ) )

(defmethod dom:previous-sibling ((node dom:node))
  (slot-value node 'previous-sibling) )

(defmethod cl:print-object ((o dom:element) s)
  (format s "#<~S ~S>" (type-of o) (node-name o))
  o )

(defmethod cl:print-object ((o name-object) s)
  (format s "#<Name-Object ~S>" (slot-value o 'qualified-name))
  o )

;;; [R]
(defmethod dom:remove-child ((node dom:node) old-child)
  (error 'dom:hierachy-error :parent-node node :node old-child) )

(defmethod dom:remove-child ((parent-node dom:parent-node) old-child)
  (unless (eq (slot-value old-child 'parent-node) parent-node)
    (error 'dom:hierachy-error :parent-node parent-node :node old-child) )
  (%remove-child parent-node old-child)
   (let* ((doc   (owner-document parent-node))
         (float (slot-value doc 'floating-node)) )
    (%append-child float old-child) ) )

(defmethod dom:replace-child
      ((parent-node dom:parent-node) new-child old-child)
  (unless (eq (slot-value old-child 'parent-node) parent-node)
    (error 'dom:hierachy-error :parent-node parent-node :node old-child) )
  (unless (eq (owner-document new-child) (owner-document parent-node))
    (error 'dom:wrong-document
        :document (owner-document parent-node)
        :node new-child) )
  (typecase new-child
    ((or dom:character-data dom:element)
      (%remove-child (slot-value new-child 'parent-node) new-child)
      (%insert-before new-child old-child)
      (%remove-child parent-node old-child)
      new-child )
    (dom:document-fragment
      (loop
        (let ((node (%remove-child new-child 'first-child)))
          (unless node
            (%remove-child parent-node old-child)
            (return new-child) )
          (%remove-child new-child node)
          (%insert-before node old-child) )) )
    (otherwise
      (error 'dom:hierachy-error
            :node new-child
            :parent-node parent-node ) )) )

(defmethod dom:replace-child ((node dom:node) new-child old-child)
    (declare (ignore new-child))
  (error 'dom:hierachy-error :parent-node node :node old-child) )

;;; [S]
(defmethod dom:serialize ((attr dom:attribute) sout)
  (write-string (slot-value (slot-value attr 'name) 'qualified-name) sout)
  (write-string "=\"" sout)
  (multiple-value-bind (data start end)
      (si::string-data (dom:node-value attr))
    (let ((index start))
      (loop
        (when (eql index end) (return))
        (let ((ch (schar data index)))
          (case ch
            ((#\") (write-string "&quot;" sout))
            ((#\&) (write-string "&amp;" sout))
            ((#\<) (write-string "&lt;" sout))
            ((#\>) (write-string "&gt;" sout))
            (otherwise (write-char ch sout)) ) )
        (incf index) ) ) )
  (write-char #\" sout)
  attr )

(defmethod dom:serialize ((cd dom:character-data) sout)
  (multiple-value-bind (data start end)
      (si::string-data (slot-value cd 'data))
    (let ((index start))
      (loop
        (when (eql index end) (return cd))
        (let ((ch (schar data index)))
          (case ch
            ((#\<) (write-string "&lt;" sout))
            ((#\>) (write-string "&gt;" sout))
            (otherwise (write-char ch sout)) ) )
        (incf index) ) ) ) )

(defmethod dom:serialize ((comment dom:comment) sout)
  (write-string "<!--" sout)
  (write-string (slot-value comment 'data) sout)
  (write-string "-->" sout)
  comment )

(defmethod dom:serialize ((elt dom:element) sout)
  (write-char #\< sout)
  (write-string (slot-value (slot-value elt 'name) 'qualified-name) sout)
  (let ((runner (slot-value elt 'first-child)))
    (loop
      (unless (typep runner 'dom:attribute) (return))
      (write-char #\Space sout)
      (dom:serialize runner sout)
      (setq runner (slot-value runner 'next-sibling)) )
    (if (null runner)
        (write-string "/>" sout)
      (progn
        (write-char #\> sout)
        (loop
          (unless runner (return))
          (dom:serialize runner sout)
          (setq runner (slot-value runner 'next-sibling)) )
        (write-string "</" sout)
        (write-string (slot-value (slot-value elt 'name) 'qualified-name) sout)
        (write-char #\> sout) ))
    elt ) )

(defmethod dom:serialize ((parent-node dom:parent-node) sout)
  (let ((runner (slot-value parent-node 'first-child)))
    (loop
      (unless runner (return))
      (dom:serialize runner sout)
      (setq runner (slot-value runner 'next-sibling)) ) ) )

;;; [T]
(defmethod dom:text-content ((cd dom:character-data) sout)
  (write-string (slot-value cd 'data) sout)
  cd )

(defmethod dom:text-content ((elt dom:element) sout)
  (let ((runner (slot-value elt 'first-child)))
    (loop
      (unless (typep runner 'dom:attribute) (return))
      (setq runner (slot-value runner 'next-sibling)) )
    (loop
      (unless runner (return))
      (dom:text-content runner sout)
      (setq runner (slot-value runner 'next-sibling)) )
    elt ) )

(defmethod dom:text-content ((parent-node dom:parent-node) sout)
  (let ((runner (slot-value parent-node 'first-child)))
    (loop
      (unless runner (return))
      (dom:text-content runner sout)
      (setq runner (slot-value runner 'next-sibling)) )
    parent-node ) )
