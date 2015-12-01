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
    (let ((text (elt (plump:children node) 0)))
      (output (format nil "<issueref id=~S>" (plump:text text)))
      (plump:remove-child text)))))

(define-mode ("endissue")
  :callbacks
  (((node)
    (let ((text (elt (plump:children node) 0)))
      (plump:remove-child text))
    (output "</issueref>"))))

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
;;;   \itemitem{something}
;;;     its definition
;;;   \itemitem{something else}
;;;     what it's about
;;; \endlist
;;;
;;; So, \item is a regular list, while \itemitem is a definition list. And how
;;; do we actually emit code? Well, we have to look at the siblings of a
;;; \beginlist element.

(defparameter *list-context* (list))

(defun siblings-until (node test)
  "Find all siblings until a certain element."
  (let ((siblings (plump:children (plump:parent node)))
        (start-pos nil)
        (end-pos nil))
    (loop for i from 0 to (1- (length siblings)) do
      (let ((sibling (elt siblings i)))
        (when (eq node sibling)
          ;; Found the start position
          (setf start-pos i))
        (when (funcall test sibling)
          ;; Found the end node
          (setf end-pos i))))
    (unless end-pos
      ;; If we didn't find the end position, set it to the last element
      (setf end-pos (1- (length siblings))))
    (subseq siblings (1+ start-pos) end-pos)))

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
        (let ((nodes (siblings-until node #'list-end-p)))
          ;; `nodes` is every node between the beginlist and endlist, excluding
          ;; the endings
          ))))))
