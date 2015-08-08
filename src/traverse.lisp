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

(defparameter *emitters* (make-hash-table :test #'equal))

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

(defmethod %traverse ((node plump:nesting-node))
  (loop for child across (plump:children node) do
    (%traverse child)))

(defmethod %traverse ((node plump:element))
  (let ((emitter (gethash (plump:tag-name node) *emitters*)))
    (when emitter
      (funcall (getf emitter :before)))
    (if (and emitter
             (getf emitter :requires-body))
        ;; Only call it if it has an explicit body
        (when (> (length (plump:children node)) 0)
          (call-next-method))
        ;; Go through the children
        (call-next-method))
    (when emitter
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
