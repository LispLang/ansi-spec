;;;; Tex math
(in-package :ansi-spec.traverse)

(define-alias "ldots" "\\ldots")

(define-alias "le" "\\le")

(define-alias "leq" "\\leq")

(define-alias "geq" "\\geq")

;; just silence or silly implement all the formula stuff
(define-alias "xparen" "")

(define-mode ("down")
             :callbacks
             ((()
               ))
             :after
             (()
              (output "↓")))

(define-mode ("curly")
             :callbacks
             ((()
               (output "{ ")
              ))
             :after
             (()
              (output " }"))
              )
