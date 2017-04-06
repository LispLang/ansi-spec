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
  (define-clref-mode "conref")
  (define-clref-mode "varref")

  ;; FIXME: add a bunch
  )

(define-string-mode "term" "<term name=\"" "\" />")
(define-string-mode "newterm" "<term type=\"new\" name=\"" "\" />")

(define-mode ("newtermidx")
             :callbacks
             (((node)
               ;; (format t "node: ~a" (plump:serialize (plump:parent node)))
               (output (format nil "<term type=\"new\" text=~s"
                               (plump:text node)))
              (plump:clear node))
              ((node)
               (output (format nil " name=~s" (plump:text node)))
               (plump:clear node))
              )
             :after
             (()
              (output " />")))

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

(define-alias "lbr" "{")
(define-alias "rbr" "}")
(define-alias "hat" "^")
(define-alias "dot" ".")
(define-alias "vert" "|")
(define-alias "centerdot" "·")
;(define-alias "surd" "√") not used

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

(defmacro define-doc-shorthand (tag expansion)
  `(loop for capitalize in (list t nil) do
    (define-macro (if capitalize
                      ,(concatenate 'string
                                    (string (char-upcase (elt tag 0)))
                                    (subseq tag 1))
                      ,tag)
        (concatenate 'string
                     (string
                      (if capitalize
                          ,(char-upcase (elt expansion 0))
                          ,(elt expansion 0)))
                     ,(subseq expansion 1)))))

(define-doc-shorthand "seefun"
  "see the \term{function} \funref{#1}")

(define-doc-shorthand "seefuns"
  "see the \term{functions} \funref{#1}")

(define-doc-shorthand "seespec"
  "see the \term{special operator} \specref{#1}")

(define-doc-shorthand "seemac"
  "see the \term{macro} \macref{#1}")

(define-doc-shorthand "seevar"
  "see the \term{variable} \varref{#1}")

(define-doc-shorthand "seetype"
  "see the \term{type} \typeref{#1}")

(define-doc-shorthand "seemisc"
  "see \miscref{#1}")

(define-doc-shorthand "seesection"
  "see \secref{#1}")

(define-doc-shorthand "seechapter"
  "see \chapref{#1}")

(define-doc-shorthand "seefigure"
  "see \figref#1")

(define-doc-shorthand "seeterm"
  "see \term{#1}")

(define-doc-shorthand "seetermAlso"
  "see also \term{#1}")

(define-macro "eval"
  "evaluated")

(define-macro "noeval"
  "not evaluated")

(define-macro "evalspecial"
  "evaluated as described below")

(define-macro "Thenextfigure"
  "Figure \\chapno -- \\capno")

(define-macro "thenextfigure"
  "Figure \\chapno -- \\capno")

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

(define-trivial-mode "editornote" "editornote")

;;; Glossary

(define-alias "harda" "ā")

(define-alias "nasala" "â")

(define-alias "softa" "ä")

(define-alias "harde" "ē")

(define-alias "hardi" "ī")

(define-alias "hardo" "ō")

(define-alias "hardp" "p̄")

(define-alias "nasalo" "ô")

(define-macro "meaning"
  "$\sub{#1}$")

(define-macro "schwa" "FIXME")
(define-macro "stress" "FIXME")
(define-macro "Stress" "FIXME")
(define-macro "pronounced" "FIXME")

;;; From concept-glossary.tex

(define-macro "gentry" "<entry>")

(define-macro "gexample"
  "``#1''")

(define-macro "indextab"
  "FIXME")

(define-macro "firstindextab"
  "FIXME")

(define-macro "indextabnote"
  "FIXME")

(define-macro "Noun" "\\i{n.}")
(define-macro "Verb" "\\i{v.}")
(define-macro "TransitiveVerb" "\\i{v.t.}")
(define-macro "Adjective" "\\i{adj.}")
(define-macro "Adverb" "\\i{adv.}")

(define-macro "ANSI" "\\i{ANSI}")
(define-macro "IEEE" "\\i{IEEE}")
(define-macro "ISO" "\\i{ISO}")
(define-macro "Traditional" "\\i{Trad.}")
(define-macro "Mathematics" "\\i{Math.}")
(define-macro "Idiomatic" "\\i{Idiom.}")
(define-macro "Computers" "\\i{Comp.}")
