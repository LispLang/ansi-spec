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

;FIXME: remove these \\rm's
(define-macro "clisp" "\\rm Common Lisp")
(define-macro "Lisp" "\\rm Lisp")
(define-macro "maclisp" "\\rm MacLisp")
(define-macro "apl" "\\rm APL")
(define-macro "lmlisp" "\\rm ZetaLisp")
(define-macro "scheme" "\\rm Scheme")
(define-macro "interlisp" "\\rm InterLisp")
(define-macro "slisp" "\\rm Spice Lisp")
(define-macro "newlisp" "\\rm Nil")
(define-macro "sOnelisp" "\\rm S-1 Common Lisp")
(define-macro "fortran" "\\rm Fortran")
(define-macro "stdlisp" "\\rm Standard Lisp")
(define-macro "psl" "\\rm Portable Standard Lisp")
(define-macro "Unix" "\\rm Unix")
(define-macro "algol" "\\tt Algol")
(define-macro "TopsTwenty" "\\tt TOPS-20")

;;; Important names

(define-macro "t"
  "\\misc{t}")

(define-macro "nil"
  "\\misc{nil}")

(define-macro "empty"
  "\\c{()}")

(define-macro "allowotherkeys"
  "\\keyword{&allow-other-keys}")

(define-macro "aux"
  "\\keyword{&aux}")

(define-macro "body"
  "\\keyword{&body}")

(define-macro "environment"
  "\\keyword{&environment}")

(define-macro "key"
  "\\keyword{&key}")

(define-macro "opt"
  "\\keyword{&optional}")

(define-macro "optional"
  "\\keyword{&optional}")

(define-macro "rest"
  "\\keyword{&rest}")

(define-macro "whole"
  "\\keyword{&whole}")

;;; General phrases

(define-macro "etc." "\\it{etc.}")
(define-macro "ie" "\\it{i.e.}, ")
(define-macro "eg" "\\it{e.g.}, ")

;;; Domain-specific phrases

(define-macro "defmethod" "defmethod")
(define-macro "CLOS" "object system")
(define-macro "OS" "object system")

(define-macro "SETFof"
  "\\macref{setf} of \\misc{#1}")

(define-macro "objectoftype"
  "\\term{object} of \\term{type} \\f{#1}")

(define-macro "objectsoftype"
  "\\term{objects} of \\term{type} \\f{#1}")

(define-macro "Objectsoftype"
  "\\term{Objects} of \\term{type} \\f{#1}")

(define-macro "oftype"
  "of \\term{type} \\typeref{#1}")

(define-macro "ofclass"
  "of \\term{class} \\typeref{#1}")

(define-macro "oftypes"
  "of \\term{type} \\typeref{#1} or a \\term{subtype} of \\term{type} \\typeref{#1}")
(define-macro "ofmetaclass"
  "of \\term{metaclass} \\typeref{#1}")
(define-macro "thetype"
  "the \\term{type} \\typeref{#1}")
(define-macro "Thetype"
  "The \\term{type} \\typeref{#1}")
(define-macro "thetypes"
  "the \\term{types} \\typeref{#1}")
(define-macro "Thetypes"
  "The \\term{types} \\typeref{#1}")
(define-macro "theclass"
  "the \\term{class} \\typeref{#1}")
(define-macro "Theclass"
  "The \\term{class} \\typeref{#1}")
(define-macro "thevariable" "the \\term{variable} \\varref{#1}")
(define-macro "Thevariable" "The \\term{variable} \\varref{#1}")
(define-macro "thevariables" "the \\term{variables} \\varref{#1}")
(define-macro "Thevariables" "The \\term{variables} \\varref{#1}")
(define-macro "themacro" "the \\funref{#1} \\term{macro}")
(define-macro "Themacro" "The \\funref{#1} \\term{macro}")
(define-macro "theinitkeyarg" "the \\kwd{#1} initialization argument")
(define-macro "Theinitkeyarg" "The \\kwd{#1} initialization argument")
(define-macro "theinitkeyargs" "the initialization arguments named \\kwd{#1}")
(define-macro "Theinitkeyargs" "The initialization argument named \\kwd{#1}")
(define-macro "thekeyarg" "the \\kwd{#1} \\term{argument}")
(define-macro "Thekeyarg" "The \\kwd{#1} \\term{argument}")
(define-macro "thefunction" "the \\term{function} \\funref{#1}")
(define-macro "Thefunction" "The \\term{function} \\funref{#1}")
(define-macro "thefunctions" "the \\term{functions} \\funref{#1}")
(define-macro "Thefunctions" "The \\term{functions} \\funref{#1}")
(define-macro "thespecform" "the \\specref{#1} \\term{special form}")
(define-macro "Thespecform" "The \\specref{#1} \\term{special form}")
(define-macro "thespecforms" "the \\specref{#1} \\term{special forms}")
(define-macro "Thespecforms" "The \\specref{#1} \\term{special forms}")
(define-macro "thespecop" "the \\specref{#1} \\term{special operator}")
(define-macro "Thespecop" "The \\specref{#1} \\term{special operator}")
(define-macro "Thespecforms" "The \\specref{#1} \\term{special forms}")
(define-macro "theGF" "the \\term{generic function} \\funref{#1}")
(define-macro "TheGF" "The \\term{generic function} \\funref{#1}")
(define-macro "subtypeof" "\\term{subtype} of \\term{type} \\typeref{#1}")
(define-macro "subtypesof" "\\term{subtypes} of \\term{type} \\typeref{#1}")
(define-macro "Subtypesof" "\\term{Subtypes} of \\term{type} \\typeref{#1}")
(define-macro "supertypeof" "\\term{supertype} of \\term{type} \\typeref{#1}")
(define-macro "supertypesof" "\\term{supertypes} of \\term{type} \\typeref{#1}")
(define-macro "Supertypesof" "\\term{Supertypes} of \\term{type} \\typeref{#1}")
(define-macro "subclassof" "\\term{subclass} of \\term{class} \\typeref{#1}")
(define-macro "subclassesof" "\\term{subclasses} of \\term{class} \\typeref{#1}")
(define-macro "Subclassesof" "\\term{Subclasses} of \\term{class} \\typeref{#1}")
(define-macro "superclassof" "\\term{superclass} of \\term{class} \\typeref{#1}")
(define-macro "superclassesof" "\\term{superclasses} of \\term{class} \\typeref{#1}")
(define-macro "Superclassesof" "\\term{Superclasses} of \\term{class} \\typeref{#1}")
(define-macro "therestart" "the \\misc{#1} \\term{restart}")
(define-macro "Therestart" "The \\misc{#1} \\term{restart}")
(define-macro "thepackage" "the \\packref{#1} \\term{package}")
(define-macro "Thepackage" "The \\packref{#1} \\term{package}")
(define-macro "instofclass" "\\term{instance} of the \\term{class} \\typeref{#1}")
(define-macro "instsofclass" "\\term{instances} of the \\term{class} \\typeref{#1}")
(define-macro "Instsofclass" "\\term{Instances} of the \\term{class} \\typeref{#1}")
(define-macro "instanceofclasses" "\\term{generalized instance} of \\theclass{#1}")
(define-macro "instancesofclasses" "\\term{generalized instances} of \\theclass{#1}")
(define-macro "Instancesofclasses" "\\term{Generalized instances} of \\theclass{#1}")
(define-macro "Theloopconstruct" "The \\macref{loop} \\loopref{#1} construct}")
(define-macro "theloopconstruct" "the \\macref{loop} \\loopref{#1} construct}")
(define-macro "Theloopkeyword" "The \\macref{loop} \\loopref{#1} keyword}")
(define-macro "theloopkeyword" "the \\macref{loop} \\loopref{#1} keyword}")
(define-macro "thevalueof" "the \\term{value} of \\misc{#1}")
(define-macro "Thevalueof" "The \\term{value} of \\misc{#1}")
(define-macro "thevaluesof" "the \\term{values} of \\misc{#1}")
(define-macro "Thevaluesof" "The \\term{values} of \\misc{#1}")
(define-macro "formatOp" "\\dummy\\hbox{{\\tt ~#1}}")
(define-macro "formatdirective" "{\\dummy}\\hbox{{\\tt ~#1}} format directive")
(define-macro "NamedTypePredicate" "{\\funref{#1} returns \\term{true} if \\param{#2} is \\oftype{#3}; otherwise, it returns \\term{false}.}")
(define-macro "TypePredicate"
  "Returns \\term{true} if \\param{#1} is \\oftype{#2}; otherwise, returns \\term{false}.")
(define-macro "NamedPredicate"
  "\\funref{#1} returns \\term{true} if \\param{#2} is #3; otherwise, returns \\term{false}.")
(define-macro "Predicate"
  "Returns \\term{true} if \\param{#1} is #2; otherwise, returns \\term{false}.")
(define-macro "StrictPredicate"
  "Returns \\misc{t} if \\param{#1} is #2; otherwise, returns \\misc{nil}.")
(define-macro "Shouldcheckplus"
  "Should signal an error \\oftype{program-error} if at least one \\param{#1} is not supplied.")
(define-macro "Checktype"
  "Signals an error \\oftype{type-error} if \\param{#1} is not #2.")
(define-macro "Checktypes"
  "Signals an error \\oftype{type-error} if #1 are not #2.")
(define-macro "Checknottype"
  "Signals an error \\oftype{type-error} if \\param{#1} is #2.")
(define-macro "Checknottypes"
  "Signals an error \\oftype{type-error} if #1 are #2.")
(define-macro "Checkanytype"
  "Signals an error \\oftype{type-error} if any \\param{#1} is not #2.")
(define-macro "Shouldchecktype"
  "Should signal an error \\oftype{type-error} if \\param{#1} is not #2.")
(define-macro "Shouldcheckanytype"
  "Should signal an error \\oftype{type-error} if any \\param{#1} is not #2.")
(define-macro "Lazychecktype"
  "Should be prepared to signal an error \\oftype{type-error} if \\param{#1} is not #2.")
(define-macro "Lazychecktypes"
  "Should be prepared to signal an error \\oftype{type-error} if #1 are not #2.")
(define-macro "Lazychecknottype"
  "Should be prepared to signal an error \\oftype{type-error} if \\param{#1} is #2.")
(define-macro "Lazycheckanytype"
  "Should be prepared to signal an error \\oftype{type-error} if any \\param{#1} is not #2.")
(define-macro "Lazycheckanynottype"
  "Should be prepared to signal an error \\oftype{type-error} if any \\param{#1} is #2.")
(define-macro "checktype"
  "signals an error \\oftype{type-error} if \\param{#1} is not #2.")
(define-macro "checkanytype"
  "signals an error \\oftype{type-error} if any \\param{#1} is not #2.")
(define-macro "shouldchecktype"
  "should signal an error \\oftype{type-error} if \\param{#1} is not #2.")
(define-macro "shouldcheckanytype"
  "should signal an error \\oftype{type-error} if any \\param{#1} is not #2.}")
(define-macro "lazychecktype"
  "should be prepared to signal an error \\oftype{type-error} if \\param{#1} is not #2.}")
(define-macro "lazycheckanytype"
  "should be prepared to signal an error \\oftype{type-error} if any \\param{#1} is not #2.}")
(define-macro "Default"
  "The default is #1.")
(define-macro "DefaultFor"
  "The default for #1 is #2.")
(define-macro "DefaultIn"
  "The default in #1 is #2.")
(define-macro "Defaults"
  "The defaults for #1 are #2, respectively.")
(define-macro "DefaultEach"
  "The defaults for each of #1 is #2.")
(define-macro "DefaultsIn"
  "The defaults for #2 in #1 are #3, respectively.")
(define-macro "HairyDefault"
  "Complicated defaulting behavior; see below")
(define-macro "MentionMetaObjects"
  "\\issue{SLOT-VALUE-METACLASSES:LESS-MINIMAL}
  Although no \\term{implementation} is required to do so,
  implementors are strongly encouraged to implement \\thefunction{#1} using
  the \\term{function} \\f{#2} described in the \\term{Metaobject Protocol}.
\\endissue{SLOT-VALUE-METACLASSES:LESS-MINIMAL}}")
(define-macro "symbolnamedesignator"
  "string designator")
(define-macro "symbolnamedesignators"
  "string designators")
(define-macro "packagenamedesignator"
  "string designator")
(define-macro "packagenamedesignators"
  "string designators")
