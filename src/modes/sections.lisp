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
  (list "beginSection"
        "beginsubsection"
        "beginsubSection"
        "beginsubsubsection"
        "beginsubsubsubsection"
        "beginsubsubsubsubsection"))

(defparameter +end-section-tags+
  (list "endSection"
        "endsubsection"
        "endsubSection"
        "endsubsubsection"
        "endsubsubsubsection"
        "endsubsubsubsubsection"))

(loop for tag in +start-section-tags+ do
  (define-mode (tag)
    :callbacks
    ((()
      ;;; Callback for the `\begin...` tag
      (output (format nil "<section>~%<title>")))
     (()
      ;;; Callback called on the `\DefineSection` tag
      (output "</title>")))))

(define-mode ("DefineSection")
  ;; Do nothing
  )

(loop for tag in +end-section-tags+ do
  (define-mode (tag)
    :callbacks
    ((()
      (output "</section>")))))

;;; Chapters

(define-mode ("beginchapter")
    :callbacks
  (((node)
    (output (format nil "<chapter index=~S id=~S ref=~S>"
                    (plump:attribute node "index")
                    (plump:attribute node "id")
                    (plump:attribute node "ref"))))))

(define-trivial-mode "chaptertitle" "title")

(define-alias "endchapter" "</chapter>")
