;;;; Tex math
(in-package :ansi-spec.traverse)

(define-alias "ldots" "\\ldots")

(define-alias "le" "\\le")

(define-alias "leq" "\\leq")

(define-alias "geq" "\\geq")

;; just silence or silly implement all the formula stuff
(define-alias "xparen" "")
(define-alias "lparen" " ( ")
(define-alias "rparen" " ) ")
(define-alias "langle" " &lt; ")
(define-alias "rangle" " &gt; ")
(define-alias "dots" " ... ")
(define-alias "neq" " != ")

;; yeah, lot's of duplication
(define-mode ("auxbnf")
             :callbacks
             ((()
               )
              (()
               (output " ::= ")
               ))
             )

(define-mode ("down")
             :callbacks
             ((()
               ))
             :after
             (()
              (output "↓")))

(define-mode ("star")
             :callbacks
             ((()
               ))
             :after
             (()
              (output "*")))

(define-mode ("plus")
             :callbacks
             ((()
               ))
             :after
             (()
              (output "+")))

(define-mode ("curly")
             :callbacks
             ((()
               (output "{ ")
              ))
             :after
             (()
              (output " }"))
              )

(define-mode ("ttbrac")
             :callbacks
             ((()
               (output "[ ")
               ))
             :after
             (()
              (output " ]"))
             )

(define-mode ("brac")
             :callbacks
             ((()
               (output "[ ")
               ))
             :after
             (()
              (output " ]"))
             )

(define-mode ("paren")
             :callbacks
             ((()
               (output "( ")
               ))
             :after
             (()
              (output " )"))
             )
