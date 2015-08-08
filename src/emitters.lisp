(in-package :ansi-spec.traverse)

;;;; Formatting

(define-emitter ("b" :requires-body t)
                :before "<b>"
                :after "</b>")

;;;; Sections

(define-emitter ("beginchapter")
                :before "<chapter ")

(define-emitter ("beginchapterindex")
                :before "index=\""
                :after "\"")

(define-emitter ("beginchaptertitle")
                :before "title=\""
                :after "\"")

(define-emitter ("beginchapterid")
                :before "id=\""
                :after "\"")

(define-emitter ("beginchapterreftitle")
                :before "ref-title=\""
                :after "\"")

(define-emitter ("endchapter")
  :before "</chapter>")
