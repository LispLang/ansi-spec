;;;; setup-terms.tex
;;;; User-defined macros
(in-package :ansi-spec.traverse)

(defparameter *macros*
  (make-hash-table :test #'equal)
  "A map of macro names to their result string.")

(defmacro define-macro (tag-name source)
  `(progn
     (setf (gethash ,tag-name *macros*)
           ,source)
     (define-alias ,tag-name
       ,(format nil "<macro id=~S/>" tag-name))))

;;; References

;; Books

(define-macro "CLtL"
  "<i>Common Lisp: The Language</i>")

(define-macro "CLtLTwo"
  "<i>Common Lisp: The Language, Second Edition</i>")

(define-macro "RandomHouseDictionary"
  "<i>The Random House Dictionary of
 the English Language, Second Edition, Unabridged</i>")

(define-macro "WebstersDictionary"
  "<i>Webster's Third New International Dictionary
 the English Language, Unabridged</i>")

(define-macro "CondSysPaper"
  "<i>Exceptional Situations in Lisp</i>")

(define-macro "GabrielBenchmarks"
  "<i>Performance and Evaluation of Lisp Programs</i>")

(define-macro "KnuthVolThree"
  "<i>The Art of Computer Programming, Volume 3</i>")

(define-macro "MetaObjectProtocol"
  "<i>The Art of the Metaobject Protocol</i>")

(define-macro "AnatomyOfLisp"
  "<i>The Anatomy of Lisp</i>")

(define-macro "FlavorsPaper"
  "<i>Flavors: A Non-Hierarchical Approach to Object-Oriented Programming</i>")

(define-macro "LispOnePointFive"
  "<i>Lisp 1.5 Programmer's Manual</i>")

(define-macro "Moonual"
  "<i>Maclisp Reference Manual, Revision 0</i>")

(define-macro "Pitmanual"
  "<i>The Revised Maclisp Manual</i>")

(define-macro "InterlispManual"
  "<i>Interlisp Reference Manual</i>")

(define-macro "Chinual"
  "<i>Lisp Machine Manual</i>")

(define-macro "SmalltalkBook"
  "<i>Smalltalk-80: The Language and its Implementation</i>")

(define-macro "XPPaper"
  "<i>XP: A Common Lisp Pretty Printing System</i>")

;; Standards

(define-macro "IEEEFloatingPoint"
  "<i>IEEE Standard for Binary Floating-Point Arithmetic</i>")

(define-macro "IEEEScheme"
  "<i>IEEE Standard for the Scheme Programming Language</i>")

;; FIXME: what does \rm do?
(define-macro "ISOChars" "ISO 6937/2")

;; Papers

(define-macro "PrincipalValues" "Principal Values and Branch Cuts in Complex APL")

(define-macro "RevisedCubedScheme" "Revised$^3$ Report on the Algorithmic Language Scheme")

(define-macro "StandardLispReport" "Standard LISP Report")

(define-macro "NILReport" "NIL---A Perspective")

(define-macro "SOneCLPaper" "S-1 Common Lisp Implementation")

(define-macro "CLOSPaper" "Common Lisp Object System Specification")

;;; Languages, OSs, etc.

;FIXME: remove these \rm's
(define-macro "clisp" "\rm Common Lisp")
(define-macro "Lisp" "\rm Lisp")
(define-macro "maclisp" "\rm MacLisp")
(define-macro "apl" "\rm APL")
(define-macro "lmlisp" "\rm ZetaLisp")
(define-macro "scheme" "\rm Scheme")
(define-macro "interlisp" "\rm InterLisp")
(define-macro "slisp" "\rm Spice Lisp")
(define-macro "newlisp" "\rm Nil")
(define-macro "sOnelisp" "\rm S-1 Common Lisp")
(define-macro "fortran" "\rm Fortran")
(define-macro "stdlisp" "\rm Standard Lisp")
(define-macro "psl" "\rm Portable Standard Lisp")
(define-macro "Unix" "\rm Unix")
(define-macro "algol" "\tt Algol")
(define-macro "TopsTwenty" "\tt TOPS-20")

;;; Important names

;;; General phrases

(define-macro "etc." "<i>etc.</i>")
(define-macro "ie" "<i>i.e.</i>, ")
(define-macro "eg" "<i>e.g.</i>, ")

;;; Domain-specific phrases

(define-macro "defmethod" "defmethod")
(define-macro "CLOS" "object system")
(define-macro "OS" "object system")
