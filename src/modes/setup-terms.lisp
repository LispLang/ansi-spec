;;;; setup-terms.tex
;;;; User-defined macros
(in-package :ansi-spec.traverse)

(defparameter *terms*
  (make-hash-table :test #'equal)
  "A map of term names to their result string.")

(defmacro define-term (tag-name source)
  `(progn
     (setf (gethash ,tag-name *terms*)
           ,source)
     (define-alias ,tag-name
       ,(format nil "<macro id=~S/>" tag-name))))

;;; References

;; Books

(define-term "CLtL"
  "<i>Common Lisp: The Language</i>")

(define-term "CLtLTwo"
  "<i>Common Lisp: The Language, Second Edition</i>")

(define-term "RandomHouseDictionary"
  "<i>The Random House Dictionary of
 the English Language, Second Edition, Unabridged</i>")

(define-term "WebstersDictionary"
  "<i>Webster's Third New International Dictionary
 the English Language, Unabridged</i>")

(define-term "CondSysPaper"
  "<i>Exceptional Situations in Lisp</i>")

(define-term "GabrielBenchmarks"
  "<i>Performance and Evaluation of Lisp Programs</i>")

(define-term "KnuthVolThree"
  "<i>The Art of Computer Programming, Volume 3</i>")

(define-term "MetaObjectProtocol"
  "<i>The Art of the Metaobject Protocol</i>")

(define-term "AnatomyOfLisp"
  "<i>The Anatomy of Lisp</i>")

(define-term "FlavorsPaper"
  "<i>Flavors: A Non-Hierarchical Approach to Object-Oriented Programming</i>")

(define-term "LispOnePointFive"
  "<i>Lisp 1.5 Programmer's Manual</i>")

(define-term "Moonual"
  "<i>Maclisp Reference Manual, Revision 0</i>")

(define-term "Pitmanual"
  "<i>The Revised Maclisp Manual</i>")

(define-term "InterlispManual"
  "<i>Interlisp Reference Manual</i>")

(define-term "Chinual"
  "<i>Lisp Machine Manual</i>")

(define-term "SmalltalkBook"
  "<i>Smalltalk-80: The Language and its Implementation</i>")

(define-term "XPPaper"
  "<i>XP: A Common Lisp Pretty Printing System</i>")

;; Standards

(define-term "IEEEFloatingPoint"
  "<i>IEEE Standard for Binary Floating-Point Arithmetic</i>")

(define-term "IEEEScheme"
  "<i>IEEE Standard for the Scheme Programming Language</i>")

;; FIXME: what does \rm do?
(define-term "ISOChars" "ISO 6937/2")

;; Papers

(define-term "PrincipalValues" "Principal Values and Branch Cuts in Complex APL")

(define-term "RevisedCubedScheme" "Revised$^3$ Report on the Algorithmic Language Scheme")

(define-term "StandardLispReport" "Standard LISP Report")

(define-term "NILReport" "NIL---A Perspective")

(define-term "SOneCLPaper" "S-1 Common Lisp Implementation")

(define-term "CLOSPaper" "Common Lisp Object System Specification")

;;; Languages, OSs, etc.

;FIXME: remove these \rm's
(define-term "clisp" "\rm Common Lisp")
(define-term "Lisp" "\rm Lisp")
(define-term "maclisp" "\rm MacLisp")
(define-term "apl" "\rm APL")
(define-term "lmlisp" "\rm ZetaLisp")
(define-term "scheme" "\rm Scheme")
(define-term "interlisp" "\rm InterLisp")
(define-term "slisp" "\rm Spice Lisp")
(define-term "newlisp" "\rm Nil")
(define-term "sOnelisp" "\rm S-1 Common Lisp")
(define-term "fortran" "\rm Fortran")
(define-term "stdlisp" "\rm Standard Lisp")
(define-term "psl" "\rm Portable Standard Lisp")
(define-term "Unix" "\rm Unix")
(define-term "algol" "\tt Algol")
(define-term "TopsTwenty" "\tt TOPS-20")

;;; Important names

;;; General phrases

(define-term "etc." "<i>etc.</i>")
(define-term "ie" "<i>i.e.</i>, ")
(define-term "eg" "<i>e.g.</i>, ")

;;; Domain-specific phrases
