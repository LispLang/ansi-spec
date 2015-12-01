(in-package :ansi-spec.traverse)

;;; Sections are defined like this:
;;;
;;; \begin[subsub...]Section{Title}
;;; \DefineSection{reference title}
;;; ...
;;; \end[subsub...]Section

(defparameter +start-section-tags+
  (list "beginSection"
        "beginsubsection"
        "beginsubsubsection"
        "beginsubsubsubsection"
        "beginsubsubsubsubsection"))

(defparameter +end-section-tags+
  (list "endSection"
        "endsubsection"
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
