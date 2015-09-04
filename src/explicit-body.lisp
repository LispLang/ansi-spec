(in-package :cl-user)
(defpackage ansi-spec.explicit-body
  (:use :cl)
  (:export :explicit-body)
  (:documentation "Explicitize bodies in a Plump node."))
(in-package :ansi-spec.explicit-body)

(defgeneric explicit-body (node)
  (:documentation "Explicitize TeX bodies."))

(defun implicit-body-p (node)
  "Take a Plump node of any kind and determine if it's an implicit body."
  (and (plump:element-p node)
       (string= (plump:tag-name node) "div")
       ;; It *might* be. Does it have chilren? Is the first child an element
       ;; with no children?
       (> (length (plump:children node)) 0)
       (let ((child (elt (plump:children node) 0)))
         (and (plump:element-p child)
              (= (length (plump:children child)) 0)))))

(defun transform (node)
  "Transform an implicit body."
  ;; Pull out the first child from the node's children
  (let ((actual-node (plump:remove-child (elt (plump:children node) 0))))
    ;; Make the node's children the the actual-node's children
    (setf (plump:children actual-node) (plump:children node))
    ;; Update the parents
    (loop for child across (plump:children actual-node) do
      (setf (plump:parent child) actual-node))
    ;; I'm forgetting something
    actual-node))

(defmethod explicit-body ((node plump:nesting-node))
  "Go through the children. If one of them can be explicitized, do it."
  (loop for child across (plump:children node) do
    (if (implicit-body-p child)
        (plump:replace-child child (transform child))))
  (loop for child across (plump:children node) do
    (explicit-body child))
  node)

(defmethod explicit-body ((node plump:text-node))
  "Do nothing."
  node)
