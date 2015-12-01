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
