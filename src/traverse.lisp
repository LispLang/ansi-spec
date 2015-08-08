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

(defmacro define-emitter ((node-name &key requires-body) &key before after)
  "Define an emitter."
  `(setf (gethash ,node-name *emitters*)
         (list :before (lambda ()
                         (emit ,before))
               :after (lambda ()
                        (when ,after
                          (emit ,after)))
               :requires-body ,requires-body)))

(defun emit (string)
  (write-string string *stream*))

;;; Entry point

(defgeneric %traverse (node))

(defmethod %traverse ((node plump:root))
  (loop for child across (plump:children node) do
    (%traverse child)))

(defmethod %traverse ((node plump:element))
  (let* ((emitter (gethash (plump:tag-name node) *emitters*))
         (can-emit (if emitter
                       (if (getf emitter :requires-body)
                           (if (> (length (plump:children node)) 0)
                               t
                               nil)
                           t)
                       nil)))
    (when can-emit
      (funcall (getf emitter :before)))
    (loop for child across (plump:children node) do
      (%traverse child))
    (when can-emit
      (funcall (getf emitter :after)))))

(defmethod %traverse ((node plump:text-node))
  (emit (plump:text node)))

(defun traverse (pathname)
  "Traverse the document in pathname."
  (format t "~&Traversing '~A.tex'" (pathname-name pathname))
  (ansi-spec.file:with-output-file (*stream*)
    (%traverse (plump-tex:parse
                (ansi-spec.preprocess:preprocess
                 (uiop:read-file-string pathname))))))
