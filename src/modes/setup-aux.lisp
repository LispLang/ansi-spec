;;;; setup-aux.tex
(in-package :ansi-spec.traverse)

(define-alias "sc" "scaled")

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
