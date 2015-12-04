;;;; setup-aux.tex
(in-package :ansi-spec.traverse)

(define-alias "sc" "scaled")

;;; Formatting

(define-trivial-mode "tt" "c")

;;; Issues
;;;
;;; References to issues look like this:
;;;
;;; \issue{name}
;;; content
;;; \endissue{name}
;;;
;;; This is to be interpreted as "[content] is caused by the issue [name]". In
;;; CLHS, these references are collected and displayed as a list of issues at
;;; the end of the page.

(define-mode ("issue")
  :callbacks
  (((node)
    (when (> (length (plump:children node)) 0)
      (let ((text (elt (plump:children node) 0)))
        (output (format nil "<issueref id=~S>" (plump:text text)))
        (plump:remove-child text))))))

(define-mode ("endissue")
  :callbacks
  (((node)
    (when (> (length (plump:children node)) 0)
      (let ((text (elt (plump:children node) 0)))
        (plump:remove-child text))
      (output "</issueref>")))))

;;; Lists
;;;
;;; Please take a seat.
;;;
;;; So, here's the deal. Unordered lists work like this:
;;;
;;; \beginlist
;;;   \item{\bull} blah blah
;;;   \item{\bull} yada yada
;;; \endlist
;;;
;;; Ordered lists work like this:
;;;
;;; \beginlist
;;;   \item{1.} blah blah
;;;   \item{2.} yada yada
;;; \endlist
;;;
;;; At this point the abyss begins to stare back.
;;;
;;; Also there's different kinds of \item tags, there's \item, \itemitem, and
;;; \itemitemitem. The last one is never actually used. So, what's the
;;; difference between the first two?
;;;
;;; Well, \item is used like this:
;;;
;;; \beginlist
;;;   \item{bull}
;;;     blah blah blah
;;;   \item{bull}
;;;     item two
;;;   ...
;;; \endlist
;;;
;;; While \itemitem is used like this:
;;;
;;; \beginlist
;;;   \item{something}
;;;     its definition
;;;   \itemitem{something else}
;;;     what it's about
;;; \endlist
;;;
;;; So, \item is a regular list, ordered or unordered, while \itemitem is a
;;; definition list, but also \item can also be a definition list. The key
;;; discriminant is in the content of the \item or \itemitem tag: If it's a
;;; string, it's an enumerated list, if it's a node that isn't \bull or
;;; something, it's a definition list, it's an ordered list of the node is \bull
;;; or something, and if the item tag is empty, then, fuck it, it's unordered,
;;; why not.
;;;
;;; And how do we actually emit code? Well, we have to look at the siblings of a
;;; \beginlist element.

(defparameter *list-context* (list))

(defun list-type-tag (list-type)
  (ccase list-type
    (:ordered "ol")
    (:unordered "ul")
    (:definition "dl")))

(define-mode ("beginlist")
  :callbacks
  (((node)
    ;; Find all nodes until the `\endlist' node
    (let ((counter 0))
      (flet ((list-end-p (node)
               (if (plump:element-p node)
                   (let ((name (plump:tag-name node)))
                     (cond
                       ((string= name "beginlist")
                        (incf counter)
                        nil)
                       ((string= name "endlist")
                        (decf counter)
                        (if (= counter 0)
                            t
                            nil))))
                   nil)))
        (let ((nodes (siblings-until node #'list-end-p))
              (list-type nil))
          ;; `nodes` is every node between the beginlist and endlist, excluding
          ;; the endings
          (loop for elem across nodes do
            (when (and (plump:element-p elem)
                       (or (string= (plump:tag-name elem) "item")
                           (string= (plump:tag-name elem) "itemitem")))
              ;; Determine the type of list
              (let ((body (plump:children elem)))
                (cond
                  ((or (uiop:emptyp body)
                       (and (= (length body) 1)
                            (let ((elem (elt body 0)))
                              (and (plump:element-p elem)
                                   (string= (plump:tag-name elem) "bull")))))
                   (setf list-type :unordered))
                  ((and (= (length body) 1)
                        (plump:text-node-p (elt body 0)))
                   (setf list-type :ordered))
                  (t
                   (setf list-type :definition))))
              (return)))
          (output (format nil "<~A>" (list-type-tag list-type)))
          (unless list-type
            (format t "~%~%~%~%~%~%Unknown list type~%~%~%~%~%~%~%~%~%"))
          (push list-type *list-context*)))))))

(defun on-list-node (node)
  (declare (ignore node))
  nil)

(define-mode ("item")
  :callbacks
  (((node)
    (on-list-node node))))

(define-mode ("itemitem")
  :callbacks
  (((node)
    (on-list-node node))))

(define-mode ("endlist")
  :callbacks
  ((()
    (let ((type (pop *list-context*)))
      (when type
        ;; There's an error in the spec with an \endlist that doesn't correspond
        ;; to any \beginlist
        (output
         (format nil "</~A>"
                 (list-type-tag type))))))))

;;; Index commands

(loop for elem in (list "ref" "keyref" "code" "kwd" "text" "term" "example" "packref") do
  (let ((fullname (concatenate 'string "idx" elem)))
    (define-trivial-mode fullname fullname)))

#|
(let ((map `((:name "ref" :type "R")
             (:name "keyref" :type "R" :pre "&")
             (:name "code" :type "C")
             (:name "kwd" :type "K")
             (:name "text" :type "T")
             (:name "term" :type "G")
             (:name "example" :type "E")
             (:name "packref" :type "P"))))
  (loop for macro in map do
    (define-mode ((concatenate 'string "idx" (getf macro :name)))
      :callbacks
      (((node)
        ;; The first node contains a letter
        (let ((letter (plump:text (elt (plump:children node) 0))))
          (output (format nil "<index type=~S " letter))))
   ((node)
    ;; This node contains the entry
    (let ((entry (plump:text (elt (plump:children node) 0))))
      (output (format nil " entry~S/>" entry))))))
|#
