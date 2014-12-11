(in-package :cl-ansi-spec)

(defgeneric filter (object)
  (:documentation "Take a Plump node from the TeX source and manipulate it."))

(defmethod filter ((text-node plump:text-node))
  (plump:text text-node))

(defmethod filter ((vector vector))
  (loop for elem across vector collecting
    (filter elem)))

(defmethod filter ((root plump:root))
  (filter (plump:children root)))

(defmethod filter ((element plump:element))
  (let ((name (plump:tag-name element))
        (attributes (plump:attributes element))
        (children (plump:children element)))
    t))
