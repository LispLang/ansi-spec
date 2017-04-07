;;;; setup-figures.tex
(in-package :ansi-spec.traverse)

(defparameter +figure-ref-captions+
  (list '("StdCharsThree" "Figure 2--5")
        '("PossibleSyntaxTypes" "Figure~2--6")
        '("CharSyntaxTypesInStdSyntax" "Figure~2--7")
        '("ConstituentTraitsOfStdChars" "Figure~2--8")
        '("SyntaxForNumericTokens" "Figure~2--9")
        '("CLSpecialOps" "Figure~3--2")
        '("TypeInfoXrefs" "Figure~4--1")
        '("StandardizedAtomicTypeSpecs" "Figure~4--2")
        '("StandardizedCompoundTypeSpecNames" "Figure~4--3")
        '("TypesAndDeclsNames" "Figure~4--5")
        '("StandardizedTypeSpecifierNames" "Figure~4--6")
        '("ObjectSystemClasses" "Figure 4--7")
        '("ClassTypeCorrespondence" "Figure 4--8")
        '("StdMethDefOps" "Figure 7--1")
        '("StandardizedConditionTypes" "Figure 9--1")
        '("SequenceFunctions" "Figure 17--1")
        '("PathnameCaseFuns" "Figure 19--2")
        '("InputStreamOps" "Figure 21--2")
        '("OutputStreamOps" "Figure 21--3")
        '("StandardizedStreamVars" "Figure 21--6")
        '("OpenOrClosedStreamOps" "Figure 21--7")
        '("StdPrinterControlVars" "Figure 22--1")))

(loop for name in (list "figref" "Figref") do
  (define-mode (name)
               :callbacks
               ((()
                 (output "<figref "))
                ((node)
                 (let* ((ref (plump:tag-name node))
                        (caption (cadr
                                  (find ref +figure-ref-captions+
                                        :key #'first
                                        :test #'string=))))
                   ;; (format t "node ~a" (plump:serialize node))
                   ;; (format t "figref ref=~s caption=~s" ref caption)
                   (output (format nil "ref=~s>~a" ref caption))
                   ;; (plump:remove-child node)
                   ))
                )
               :after
               (()
                (output "</figref>"))))
