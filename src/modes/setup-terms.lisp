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
  "\it{Common Lisp: The Language</i>")

(define-macro "CLtLTwo"
  "\it{Common Lisp: The Language, Second Edition</i>")

(define-macro "RandomHouseDictionary"
  "\it{The Random House Dictionary of
 the English Language, Second Edition, Unabridged}")

(define-macro "WebstersDictionary"
  "\it{Webster's Third New International Dictionary
 the English Language, Unabridged}")

(define-macro "CondSysPaper"
  "\it{Exceptional Situations in Lisp}")

(define-macro "GabrielBenchmarks"
  "\it{Performance and Evaluation of Lisp Programs}")

(define-macro "KnuthVolThree"
  "\it{The Art of Computer Programming, Volume 3}")

(define-macro "MetaObjectProtocol"
  "\it{The Art of the Metaobject Protocol}")

(define-macro "AnatomyOfLisp"
  "\it{The Anatomy of Lisp}")

(define-macro "FlavorsPaper"
  "\it{Flavors: A Non-Hierarchical Approach to Object-Oriented Programming}")

(define-macro "LispOnePointFive"
  "\it{Lisp 1.5 Programmer's Manual}")

(define-macro "Moonual"
  "\it{Maclisp Reference Manual, Revision 0}")

(define-macro "Pitmanual"
  "\it{The Revised Maclisp Manual}")

(define-macro "InterlispManual"
  "\it{Interlisp Reference Manual}")

(define-macro "Chinual"
  "\it{Lisp Machine Manual}")

(define-macro "SmalltalkBook"
  "\it{Smalltalk-80: The Language and its Implementation}")

(define-macro "XPPaper"
  "\it{XP: A Common Lisp Pretty Printing System}")

;; Standards

(define-macro "IEEEFloatingPoint"
  "\it{IEEE Standard for Binary Floating-Point Arithmetic}")

(define-macro "IEEEScheme"
  "\it{IEEE Standard for the Scheme Programming Language}")

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

(define-macro "etc." "\it{etc.}")
(define-macro "ie" "\it{i.e.}, ")
(define-macro "eg" "\it{e.g.}, ")

;;; Domain-specific phrases

(define-macro "defmethod" "defmethod")
(define-macro "CLOS" "object system")
(define-macro "OS" "object system")
