(in-package :cl-ansi-spec)

(defparameter *transform-table* (make-hash-table :test #'equal))

(defmacro define-transform (tag-name (attributes children) &rest body)
  `(setf (gethash ,tag-name *transform-table*)
         (lambda (,attributes ,children)
           ,@body)))

(defgeneric transform (object)
  (:documentation "Take a Plump node from the TeX source and manipulate it."))

(defmethod transform ((text-node plump:text-node))
  (plump:text text-node))

(defmethod transform ((vector vector))
  (loop for elem across vector collecting
    (transform elem)))

(defmethod transform ((root plump:root))
  (transform (plump:children root)))

(defmethod transform ((element plump:element))
  (let ((name (plump:tag-name element))
        (attributes (plump:attributes element))
        (children (plump:children element)))
    (aif (gethash name *transform-table*)
         (funcall it attributes (transform children))
         (format t "Could not find transform for tag '~A'~%" name))))

(defun attr (attrs name)
  (let ((seq (subseq (gethash name attrs) 1)))
    (subseq seq 0 (1- (length seq)))))

(define-transform "chapter" (a children)
  (list :chapter
        (list :index (attr a "index")
              :title (attr a "title")
              :chap-id (attr a "chap-id")
              :ref-title (attr a "ref-title"))
        children))
