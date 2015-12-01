;;;; setup-document.tex
(in-package :ansi-spec.traverse)

;;; Formatting

(define-trivial-mode "b" "b")
(define-trivial-mode "i" "i")
(define-trivial-mode "j" "i") ;; Spec says this is 'italic + kerning'. What.
(define-mode ("f")
  ;; f as in fixed-width, as in monospace, as in code
  :callbacks
  ((()
    (setf *transform-text* nil)
    (output "<c>")))
  :after
  (()
   (setf *transform-text* t)
   (output "</c>")))

;; FIXME: ff

(define-trivial-mode "ital" "i") ;; why
(define-trivial-mode "bold" "b") ;; a very idiosyncratic contributor?

(define-string-mode "ang"
  ;; Surrounded by angular brackets
  "<"
  ">")

(define-string-mode "flr"
  ;; Surrounded by floor characters
  "⌊"
  "⌋")

(define-trivial-mode "underlined" "u")

(define-string-mode "metavar"
  ;; A metavar is rendered as <<content>> in the CLHS. Because we're fancy, we
  ;; use guillemets.
  "«"
  "»")

(define-string-mode "metaparam"
  ;; This is a metavar with a param inside
  "«<param>"
  "»</param>")

;; FIXME: dummy

;;; References

(define-trivial-mode "clref" "ref")

(macrolet ((define-clref-mode (tag-name &key before after)
             `(define-string-mode ,tag-name
                ,(concatenate 'string
                              "<c><ref>"
                              (or before ""))
                ,(concatenate 'string
                              "</ref></c>"
                              (or after "")))))
  (define-clref-mode "ttref")

  (define-clref-mode "kwd" :before ":")
  (define-clref-mode "kwdref" :before ":#")
  (define-clref-mode "packref"
    :before "<uppercase>"
    :after "</uppercase>")
  (define-clref-mode "loopref")

  (define-clref-mode "keyref"
    :before "&")

  (define-clref-mode "typeref")
  (define-clref-mode "misc")
  (define-clref-mode "miscref")
  (define-clref-mode "declref")
  (define-clref-mode "funref")
  (define-clref-mode "macref")
  (define-clref-mode "specref")
  (define-clref-mode "varref")

  ;; FIXME: add a bunch
  )

(define-trivial-mode "term" "term")

;;; Special symbols

;; Subscripts

(macrolet ((define-sub-mode (tag-name sub)
             `(define-string-mode ,tag-name
                ""
                ,(format nil "<sub>~A</sub>" sub))))
  (define-sub-mode "ssso" "1")
  (define-sub-mode "ssst" "2")
  (define-sub-mode "ssse" "8")
  (define-sub-mode "ssss" "16")

  (define-sub-mode "sssi" "i")
  (define-sub-mode "sssk" "k")
  (define-sub-mode "sssn" "n")
  (define-sub-mode "sssx" "x")
  (define-sub-mode "sssy" "y")
  (define-sub-mode "sssz" "z"))

;; Relations

(define-string-mode "EV"
  ;; Evaluates to
  ""
  "⇒")

(define-string-mode "OV"
    ;; Alternative evaluation "or evaluates to..."
  ""
  "or ⇒")

(define-string-mode "NV"
  ;; Does not evaluate to
  ""
  "⇏")

(define-string-mode "EQ"
  ;; Equivalence
  ""
  "≡")

;; Contexts

(define-string-mode "OUT"
  ;; Output: CLHS renders this as ">>", the TeX source says this should be a
  ;; `\triangleright`. Thankfully there's a Unicode character just for this.
  ""
  "▷")

(define-trivial-mode "IN" "input"
  ;; User input. The CLHS doesn't render this at all. We can render it with
  ;; fancy CSS.
  )

;; Characters

(define-string-mode "CRLF"
    ;; A newline
  ""
  "↩")

(macrolet ((define-char-mode (tag-name name)
             `(define-string-mode ,tag-name
                (format nil "<~A" ,name)
                ">")))
  (define-char-mode "NewlineChar" "Newline")
  (define-char-mode "SpaceChar" "Space")
  (define-char-mode "TabChar" "Tab")
  (define-char-mode "ReturnChar" "Return")
  (define-char-mode "LinefeedChar" "Linefeed")
  (define-char-mode "BackspaceChar" "Backspace")
  (define-char-mode "PageChar" "Page")
  (define-char-mode "RuboutChar" "Rubout")
  (define-char-mode "WhitespaceChar" "Whitespace"))

(define-string-mode "bq"
  ;; Seriously? A macro for fucking backquotes?
  ""
  "`")

;; FIXME: others

(define-string-mode "underscore"
  ""
  "_")

(define-string-mode "lbracket"
  ""
  "<tex>[</tex>")

(define-string-mode "rbracket"
  ""
  "<tex>]</tex>")

(define-string-mode "minussign"
  ;; Sweet Jesus why does this exist
  ""
  "<tex>-</tex>")

;;; BNF

;;; BNF shorthand

;;; Document-related shorthand

(define-mode ("code")
  :callbacks
  ((()
    (setf *transform-text* nil)
    (output "<code>"))))

(define-mode ("endcode")
  :callbacks
  ((()
    (setf *transform-text* t)
    (output "</code>"))))

;;; Glossary
