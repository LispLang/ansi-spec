(in-package :ansi-spec.traverse)

;;;; Formatting

(define-emitter ("b" :requires-body t)
                :before "<b>"
                :after "</b>")

(define-emitter ("i" :requires-body t)
                :before "<i>"
                :after "</i>")

(define-emitter ("it" :requires-body t)
                :before "<i>"
                :after "</i>")

;;;; Sections

;;; Chapters

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

;;; Sections

(macrolet ((define-section-emitter (start-name end-name)
             `(progn
                (define-emitter (,start-name)
                                :before "<section title=\""
                                :after "\">")

                (define-emitter (,end-name)
                                :before "</section>")))
           (define-all-section-emitters (&rest sub-strings)
             `(progn
                ,@(loop for sub-string in sub-strings collecting
                    `(define-section-emitter
                         ,(format nil "begin~ASection" sub-string)
                         ,(format nil "end~ASection" sub-string))))))
  (define-all-section-emitters "" "sub" "subsub" "subsubsub" "subsubsubsub"))
