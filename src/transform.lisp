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
  (let ((attribute (gethash name attrs)))
    (when attribute
      (when (char= (elt attribute 0) #\')
        (setf attribute (subseq attribute 1)))
      (when (char= (elt attribute (1- (length attribute)))
                   #\')
        (setf attribute (subseq attribute 0 (1- (length attribute)))))
      attribute)))

;;; Sections

(define-transform "chapter" (a children)
  (list :chapter
        (list :index (attr a "index")
              :title (attr a "title")
              :chap-id (attr a "chap-id")
              :ref-title (attr a "ref-title"))
        children))

(defmacro define-section-transform (name keyword)
  `(define-transform ,name (a children)
     (list ,keyword
           (list :title (attr a "title")
                 :ref (attr a "ref"))
           children)))

(define-section-transform "section" :section)
(define-section-transform "subsection" :subsection)
(define-section-transform "subsubsection" :subsubsection)
(define-section-transform "subsubsubsection" :subsubsubsection)

;;; References

(define-transform "term" (a children)
  (list :term (first children)))

;;; Markup

(define-transform "doublequotes" (a children)
  (list :double-quotes (first children)))
