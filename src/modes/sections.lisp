(in-package :ansi-spec.traverse)

;;; Sections are defined like this:
;;;
;;; \begin[subsub...]Section{Title}
;;; \DefineSection{reference title}
;;; ...
;;; \end[subsub...]Section
;;;
;;; Chapters are like this:
;;;
;;; \beginchapter[index=... id=... ref=...]{}
;;; \chaptertitle{...}
;;; ...
;;; \endchapter{}

;;; Sections

(defparameter +start-section-tags+
  (list '("beginSection" "section")
        '("beginsubsection" "subsection")
        '("beginSubsection" "subsection")
        '("beginsubsubsection" "subsubsection")
        '("beginsubsubsubsection" "subsubsubsection")
        '("beginsubsubsubsubsection" "subsubsubsubsection")))

(defparameter +end-section-tags+
  (list '("endSection" "section")
        '("endsubsection" "subsection")
        '("endSubsection" "subsection")
        '("endsubsubsection" "subsubsection")
        '("endsubsubsubsection" "subsubsubsection")
        '("endsubsubsubsubsection" "subsubsubsubsection")))

(loop for tag in +start-section-tags+ do
  (let ((mname (car tag))
        (tname (cadr tag)))
    (define-mode (mname)
                 :callbacks
                 ((()
      ;;; Callback for the `\begin...` tag
                   (output (format nil "<~a title=\"" tname)))
                  (()
      ;;; Callback called on the `\DefineSection` tag
                   (output "\">"))))))

(define-mode ("DefineSection")
  ;; Do nothing
  )

(loop for tag in +end-section-tags+ do
  (let ((mname (car tag))
        (tname (cadr tag)))
    (define-mode (mname)
                 :callbacks
                 ((()
                   (output (format nil "</~a>" tname)))))))

;;; Chapters

(define-mode ("beginchapter")
    :callbacks
  (((node)
    (output (format nil "<chapter index=~S id=~S ref=~S title=~S>"
                    (plump:attribute node "index")
                    (plump:attribute node "id")
                    (plump:attribute node "ref")
                    (plump:attribute node "title"))))))

(define-alias "endchapter" "</chapter>")
