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

(defvar *emitters* (make-hash-table :test #'equal))

(defmacro define-emitter ((node node-name) &body body)
  "Define an emitter."
  `(setf (gethash ,node-name *emitters*)
         (lambda (,node)
           ,@body)))

(defun emit-node (name node)
  (let ((emitter (gethash name *emitters*)))
    (when emitter
      (funcall emitter node))))

(define-emitter (node "bye")
  (print "Bye!"))

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
