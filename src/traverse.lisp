(in-package :cl-user)
(defpackage ansi-spec.traverse
  (:use :cl)
  (:export :traverse)
  (:documentation "Traverse a TeX document, ignoring some nodes, extracting info
  from others into an output XML file."))
(in-package :ansi-spec.traverse)

;;; Output stream

(defvar *stream*)

;;; Emitting TeX elements

(defgeneric emit-node (name node)
  (:documentation "Emit a node. Specializes on the node name."))

(defmethod emit-node ((name t) node)
  (print node))

;;; Emitting text

(defun emit-text (node)
  (write-string (plump:text node) *stream*))

;;; Entry point

(defun traverse (pathname)
  "Traverse the document in pathname."
  (format t "~&Traversing '~A.tex'" (pathname-name pathname))
  (ansi-spec.file:with-output-file (*stream*)
    (plump:traverse (plump-tex:parse
                     (ansi-spec.preprocess:preprocess
                      (uiop:read-file-string pathname)))
                    #'(lambda (node)
                        (cond
                          ((plump:element-p node)
                           (emit-node (plump:tag-name node) node))
                          ((plump:text-node-p node)
                           (emit-text node)))))))
