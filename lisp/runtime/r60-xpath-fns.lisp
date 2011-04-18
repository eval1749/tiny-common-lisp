#|
http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/core.html

Definition group DocumentPosition
A bitmask indicating the relative document position of a node with respect to another node.

If the two nodes being compared are the same node, then no flags are set on the return.

Otherwise, the order of two nodes is determined by looking for common containers -- containers which contain both. A node directly contains any child nodes. A node also directly contains any other nodes attached to it such as attributes contained in an element or entities and notations contained in a document type. Nodes contained in contained nodes are also contained, but less-directly as the number of intervening containers increases.

If there is no common container node, then the order is based upon order between the root container of each node that is in no container. In this case, the result is disconnected and implementation-specific. This result is stable as long as these outer-most containing nodes remain in memory and are not inserted into some other containing node. This would be the case when the nodes belong to different documents or fragments, and cloning the document or inserting a fragment might change the order.

If one of the nodes being compared contains the other node, then the container precedes the contained node, and reversely the contained node follows the container. For example, when comparing an element against its own attribute or child, the element node precedes its attribute node and its child node, which both follow it.

If neither of the previous cases apply, then there exists a most-direct container common to both nodes being compared. In this case, the order is determined based upon the two determining nodes directly contained in this most-direct common container that either are or contain the corresponding nodes being compared.

If these two determining nodes are both child nodes, then the natural DOM order of these determining nodes within the containing node is returned as the order of the corresponding nodes. This would be the case, for example, when comparing two child elements of the same element.

If one of the two determining nodes is a child node and the other is not, then the corresponding node of the child node follows the corresponding node of the non-child node. This would be the case, for example, when comparing an attribute of an element with a child element of the same element.

If neither of the two determining node is a child node and one determining node has a greater value of nodeType than the other, then the corresponding node precedes the other. This would be the case, for example, when comparing an entity of a document type against a notation of the same document type.

If neither of the two determining node is a child node and nodeType is the same for both determining nodes, then an implementation-dependent order between the determining nodes is returned. This order is stable as long as no nodes of the same nodeType are inserted into or removed from the direct container. This would be the case, for example, when comparing two attributes of the same element, and inserting or removing additional attributes might change the order between existing attributes.

Defined Constants
DOCUMENT_POSITION_CONTAINED_BY
The node is contained by the reference node. A node which is contained is always following, too.
DOCUMENT_POSITION_CONTAINS
The node contains the reference node. A node which contains is always preceding, too.
DOCUMENT_POSITION_DISCONNECTED
The two nodes are disconnected. Order between disconnected nodes is always implementation-specific.
DOCUMENT_POSITION_FOLLOWING
The node follows the reference node.
DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC
The determination of preceding versus following is implementation-specific.
DOCUMENT_POSITION_PRECEDING
The second node precedes the reference node.
|#

(defun compare-document-position-aux (node1 node2 flags)
  (cond
    ((eq node1 node2)
      flags )

    ((eq (dom:parent-node node1) node2)
      (logior flags #x10) )    ; contained-by

    ((eq (dom:parent-node node2) node1)
      (logior flags #x08)) )   ; contains

    ((eq (dom:parent-node node1)
         (dom:parent-node node2) )
      (let ((runner (dom:first-child (parent-node node1))))
        (loop
          (cond
            ((eq runner node1)
              (return (logior flags #x02)))   ; preceding
            ((eq runner node2)
              (return (logior flags #0x4)) )) ; following
          (setq runner (slot-value runner 'next-sibling)) ) ) )

    ((contain-p node2 node1)
      (logior flags #x08) ) ; contains

    ((contains-p node1 node)
      (logior flags #x10) ) ; contained-by

    (t 0) ) ; disconnected


(defmethod next-node ((ns union-node-set))
  (let ((min-node nil))
    (dolist (member (union-node-set-members ns) min-node)
      (let ((node (node-set-current member)))
        (setq min-node
          (cond
            ((null min-node) node)
            ((null node) min-node)
            ((node< min-node node) min-node)
            (t node) )) ) ) ) )

(defmethod init-node ((ns ancestor-node-set) node)
  (setf (slot-value 'current-node) node)
  (next-node ns) )

(defmethod next-node ((ns ancestor-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (when current-node
      (setf (slot-value (ns 'curren-tnode)) (dom:next-sibling current-node))
      current-node ) ) )

(defmethod init-node ((ns ancestor-or-self-node-set) node)
  (setf (slot-value 'current-node) node) )

(defmethod init-node ((ns attriubte-node-set) node)
  (setf (slot-value 'current-node)
    (let ((first-child (slot-value node 'dom::first-child)))
      (and (typep first-child 'dom:attribute-node) first-child) )) )

(defmethod next-node ((ns attribute-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (when current-node
      (setf (slot-value 'curren-tnode)
        (let ((child (slot-value current-node 'dom::next-sibling)))
          (and (typep child 'dom:attribute-node) child) ))
      current-node ) ) )

(defmethod init-node ((ns child-node-set) node)
  (setf (slot-value ns 'current-node) (dom:first-child node)) )

(defmethod next-node ((ns child-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (when current-node
      (setf (slot-value (ns 'curren-tnode)) (dom:next-sibling current-node))
      current-node ) ) )

(defmethod init-node ((ns descendant-node-set) node)
  (setf (slot-value ns 'anchor-node) node)
  (setf (slot-value ns 'current-node) node)
  (next-node ns) )

(defmethod init-node ((ns descendant-or-self-node-set) node)
  (setf (slot-value ns 'anchor-node) node)
  (setf (slot-value ns 'current-node) node) )

(defmethod next-node ((ns descendant-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
        current-node
      (when current-node
        (setf (slot-value ns 'current-node)
          (or (dom:first-child current-node)
              (dom:next-sibling current-node)
              (let ((parent-node (dom:parent-node current-node)))
                (if (eq parent-node anchor-node)
                    nil
                    (dom:next-sibling (dom:first-child parent-node)) ) ))))) ) )

(defmethod init-node ((ns following-node-set) node)
  (setf (slot-value ns 'anchor-node) node)
  (setf (slot-value ns 'current-node) node)
  (next-node ns) )

(defmethod next-node ((ns following-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
        current-node
      (setf (slot-value ns 'current-node)
        (or (dom:first-child current-node)
            (dom:next-sibling current-node)
              (let ((parent-node (dom:parent-node current-node)))
                (if (eq parent-node anchor-node)
                    (setf (slot-value ns 'anchor-ndoe)
                       (dom:next-sibling anchor-node) )
                    (dom:next-sibling
                        (dom:first-child parent-node) )) )))) ) )

(defmethod init-node ((ns following-sibling-node-set) node)
  (setf (slot-value ns 'current-node) (dom:next-sibling node)) )

(defmethod next-node ((ns following-sibling-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
        current-node
      (when current-node
        (setf (slot-value ns 'current-node) (dom:next-sibling node)) )) ) )

(defmethod init-node ((ns parent-node-set) node)
  (setf (slot-value ns 'current-node) (dom:parent-node node)) )

(defmethod next-node ((ns parent-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
      current-node
        (when current-node
          (setf (slot-value ns 'current-node) nil) )) ) )

(defmethod init-node ((ns preceding-node-set) node)
  (setf (slot-value ns 'anchor-node) node)
  (setf (slot-value ns 'current-node) node)
  (next-node ns) )

(defmethod next-node ((ns preceding-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
        current-node
      (setf (slot-value ns 'current-node)
        (or (dom:last-child current-node)
            (dom:previous-sibling current-node)
              (let ((parent-node (dom:parent-node current-node)))
                (if (eq parent-node anchor-node)
                    (setf (slot-value ns 'anchor-ndoe)
                       (dom:previous-sibling anchor-node) )
                    (dom:previous-sibling
                        (dom:last-child parent-node) )) )))) ) )

(defmethod init-node ((ns preceding-sibling-node-set) node)
  (setf (slot-value ns 'current-node) (dom:next-sibling node)) )

(defmethod next-node ((ns preceding-sibling-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
        current-node
      (when current-node
        (setf (slot-value ns 'current-node) (dom:next-sibling node)) )) ) )

(defmethod init-node ((ns self-node-set) node)
  (setf (slot-value ns 'current-node) (dom:self-node node)) )

(defmethod next-node ((ns self-node-set))
  (let ((current-node (slot-value ns 'current-node)))
    (prog1
      current-node
        (when current-node
          (setf (slot-value ns 'current-node) nil) )) ) )
