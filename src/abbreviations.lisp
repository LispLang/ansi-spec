;;;; See setup-terms.tex
(in-package :cl-ansi-spec)

(defparameter *abbreviations* (list))

(defmacro define-abbrev (name expansion)
  (if (atom name)
      `(push (cons ,name ,expansion) *abbreviations*)
      `(progn
         ,@(loop for abbrev in name collecting
             `(define-abbrev ,abbrev ,expansion)))))

(defun expand-abbreviations (string)
  (let ((final-string string))
    (loop for (name . expansion) in *abbreviations* do
      (setf final-string
            (cl-ppcre:regex-replace-all (format nil "\\\\~A" name)
                                        final-string
                                        expansion)))
    final-string))

;;; Books

(define-abbrev "CLtL"
  "\\it{Common Lisp: The Language}")
(define-abbrev "CLtLTwo"
  "\\it{Common Lisp: The Language, Second Edition}")
(define-abbrev "RandomHouseDictionary"
  "\\it{The Random House Dictionary of the English Language, Second Edition, Unabridged}")
(define-abbrev "WebstersDictionary"
  "\\it{Webster's Third New International Dictionary the English Language, Unabridged}")
(define-abbrev "CondSysPaper"
  "\\it{Exceptional Situations in Lisp}")
(define-abbrev "GabrielBenchmarks"
  "\\it{Performance and Evaluation of Lisp Programs}")
(define-abbrev "KnuthVolThree"
  "\\it{The Art of Computer Programming, Volume 3}")
(define-abbrev "MetaObjectProtocol"
  "\\it{The Art of the Metaobject Protocol}")
(define-abbrev "AnatomyOfLisp"
  "\\it{The Anatomy of Lisp}")
(define-abbrev "FlavorsPaper"
  "\\it{Flavors: A Non-Hierarchical Approach to Object-Oriented Programming}")
(define-abbrev "LispOnePointFive"
  "\\it{Lisp 1.5 Programmer's Manual}")
(define-abbrev "Moonual"
  "\\it{Maclisp Reference Manual, Revision 0}")
(define-abbrev "Pitmanual"
  "\\it{The Revised Maclisp Manual}")
(define-abbrev "InterlispManual"
  "\\it{Interlisp Reference Manual}")
(define-abbrev "Chinual"
  "\\it{Lisp Machine Manual}")
(define-abbrev "SmalltalkBook"
  "\\it{Smalltalk-80: The Language and its Implementation}")
(define-abbrev "XPPaper"
  "\\it{XP: A Common Lisp Pretty Printing System}")

;;; Standards

(define-abbrev "IEEEFloatingPoint"
  "\\it{IEEE Standard for Binary Floating-Point Arithmetic}")
(define-abbrev "IEEEScheme"
  "\\it{IEEE Standard for the Scheme Programming Language}")
(define-abbrev "ISOChars"
  "\\rm{ISO 6937/2}")

;;; Papers

(define-abbrev "PrincipalValues"
  "Principal Values and Branch Cuts in Complex APL")
(define-abbrev "RevisedCubedScheme"
  "Revised\\sup{3} Report on the Algorithmic Language Scheme")
(define-abbrev "StandardLispReport"
  "Standard LISP Report")
(define-abbrev "NILReport"
  "NIL---A Perspective")
(define-abbrev "SOneCLPaper"
  "S-1 Common Lisp Implementation")
(define-abbrev "CLOSPaper"
  "Common Lisp Object System Specification")

;;; Languages, Operating Systems, etc.

(define-abbrev "clisp"
  "\\rm{Common Lisp}")
(define-abbrev "Lisp"
  "\\rm{Lisp}")
(define-abbrev "maclisp"
  "\\rm{MacLisp}")
(define-abbrev "apl"
  "\\rm{APL}")
(define-abbrev "lmlisp" ;; Lisp Machine Lisp
  "\\rm{ZetaLisp}")
(define-abbrev "scheme"
  "\\rm{Scheme}")
(define-abbrev "interlisp"
  "\\rm{InterLisp}")
(define-abbrev "slisp"
  "\\rm{Spice Lisp}")
(define-abbrev "newlisp"
  "\\rm{S-1 Common Lisp}")
(define-abbrev "sOnelisp"
  "\\rm{Nil}")
(define-abbrev "fortran"
  "\\rm{Fortran}")
(define-abbrev "stdlisp"
  "\\rm{Standard Lisp}")
(define-abbrev "psl"
  "\\rm{Portable Standard Lisp}")
(define-abbrev "Unix"
  "\\rm{Unix}")
(define-abbrev "algol"
  "\\tt{Algol}")
(define-abbrev "TopsTwenty"
  "\\tt{TOPS-20}")

;;; Important Names

(define-abbrev "nil"
  "\\misc{nil}")
(define-abbrev "empty"
  "\\tt{()}")
(define-abbrev "allowotherkeys"
  "\\keyword{&allow-other-keys}")
(define-abbrev "aux"
  "\\keyword{&aux}")
(define-abbrev "body"
  "\\keyword{&body}")
(define-abbrev "environment"
  "\\keyword{&environment}")
(define-abbrev "key"
  "\\keyword{&key}")
(define-abbrev "opt"
  "\\keyword{&optional}")
(define-abbrev "opt"
  "\\keyword{&optional}")
(define-abbrev "rest"
  "\\keyword{&rest}")
(define-abbrev "whole"
  "\\keyword{&whole}")

;;; General Phrases

(define-abbrev "etc"
  "\\it{etc.}")
(define-abbrev "ie"
  "\\it{i.e.}, ")
(define-abbrev "eg"
  "\\it{e.g.}, ")

;;; Domain-specific Phrases

(define-abbrev "defmethod"
  "defmethod")
(define-abbrev "CLOS"
  "object system")
(define-abbrev "OS"
  "object system")

(define-abbrev "HairyDefault"
  "Complicated defaulting behavior; see below")

(defmacro define-term (name arity expansion)
  `(define-abbrev ,name
     (lambda (match &rest regs)
       (declare (ignore match))
       (format nil ,expansion
               ,@(cond
                   ((eql arity 1)
                    `((first regs)))
                   ((eql arity 2)
                    `((first regs) (second regs)))
                   ((eql arity 3)
                    `((first regs) (second regs) (third regs))))))))

(define-term "SETFof" 1
  "\\macref{setf} of \\misc{~A}")
(define-term "objectoftype " 1
  "\\term{object} of \\term{type} \\f{~A}")
(define-term "objectsoftype " 1
  "\\term{objects} of \\term{type} \\f{~A}")
(define-term "Objectsoftype " 1
  "\\term{Objects} of \\term{type} \\f{~A}")
(define-term "oftype " 1
  "of \\term{type} \\typeref{~A}")
(define-term "ofclass " 1
  "of \\term{class} \\typeref{~A}")
(define-term "oftypes " 2
  "of \\term{type} \\typeref{~A} or a \\term{subtype} of \\term{type} \\typeref{~A}")
(define-term "ofmetaclass " 1
  "of \\term{metaclass} \\typeref{~A}")
(define-term "thetype " 1
  "the \\term{type} \\typeref{~A}")
(define-term "Thetype " 1
  "The \\term{type} \\typeref{~A}")
(define-term "thetypes " 1
  "the \\term{types} \\typeref{~A}")
(define-term "Thetypes " 1
  "The \\term{types} \\typeref{~A}")
(define-term "theclass " 1
  "the \\term{class} \\typeref{~A}")
(define-term "Theclass " 1
  "The \\term{class} \\typeref{~A}")
(define-term "thevariable " 1
  "the \\term{variable} \\varref{~A}")
(define-term "Thevariable " 1
  "The \\term{variable} \\varref{~A}")
(define-term "thevariables " 1
  "the \\term{variables} \\varref{~A}")
(define-term "Thevariables " 1
  "The \\term{variables} \\varref{~A}")
(define-term "themacro " 1
  "the \\funref{~A} \\term{macro}")
(define-term "Themacro " 1
  "The \\funref{~A} \\term{macro}")
(define-term "theinitkeyarg" 1
  "the \\kwd{~A} initialization argument")
(define-term "Theinitkeyarg" 1
  "The \\kwd{~A} initialization argument")
(define-term "theinitkeyargs" 1
  "the initialization arguments named \\kwd{~A}")
(define-term "Theinitkeyargs" 1
  "The initialization argument named \\kwd{~A}")
(define-term "thekeyarg" 1
  "the \\kwd{~A} \\term{argument}")
(define-term "Thekeyarg" 1
  "The \\kwd{~A} \\term{argument}")
(define-term "thefunction " 1
  "the \\term{function} \\funref{~A}")
(define-term "Thefunction " 1
  "The \\term{function} \\funref{~A}")
(define-term "thefunctions " 1
  "the \\term{functions} \\funref{~A}")
(define-term "Thefunctions " 1
  "The \\term{functions} \\funref{~A}")
(define-term "thespecform " 1
  "the \\specref{~A} \\term{special form}")
(define-term "Thespecform " 1
  "The \\specref{~A} \\term{special form}")
(define-term "thespecforms " 1
  "the \\specref{~A} \\term{special forms}")
(define-term "Thespecforms " 1
  "The \\specref{~A} \\term{special forms}")
(define-term "thespecop " 1
  "the \\specref{~A} \\term{special operator}")
(define-term "Thespecop " 1
  "The \\specref{~A} \\term{special operator}")
(define-term "Thespecforms " 1
  "The \\specref{~A} \\term{special forms}")
(define-term "theGF " 1
  "the \\term{generic function} \\funref{~A}")
(define-term "TheGF " 1
  "The \\term{generic function} \\funref{~A}")
(define-term "subtypeof " 1
  "\\term{subtype} of \\term{type} \\typeref{~A}")
(define-term "subtypesof " 1
  "\\term{subtypes} of \\term{type} \\typeref{~A}")
(define-term "Subtypesof " 1
  "\\term{Subtypes} of \\term{type} \\typeref{~A}")
(define-term "supertypeof " 1
  "\\term{supertype} of \\term{type} \\typeref{~A}")
(define-term "supertypesof " 1
  "\\term{supertypes} of \\term{type} \\typeref{~A}")
(define-term "Supertypesof " 1
  "\\term{Supertypes} of \\term{type} \\typeref{~A}")
(define-term "subclassof " 1
  "\\term{subclass} of \\term{class} \\typeref{~A}")
(define-term "subclassesof " 1
  "\\term{subclasses} of \\term{class} \\typeref{~A}")
(define-term "Subclassesof " 1
  "\\term{Subclasses} of \\term{class} \\typeref{~A}")
(define-term "superclassof " 1
  "\\term{superclass} of \\term{class} \\typeref{~A}")
(define-term "superclassesof " 1
  "\\term{superclasses} of \\term{class} \\typeref{~A}")
(define-term "Superclassesof " 1
  "\\term{Superclasses} of \\term{class} \\typeref{~A}")
(define-term "therestart " 1
  "the \\misc{~A} \\term{restart}")
(define-term "Therestart " 1
  "The \\misc{~A} \\term{restart}")
(define-term "thepackage " 1
  "the \\packref{~A} \\term{package}")
(define-term "Thepackage " 1
  "The \\packref{~A} \\term{package}")
(define-term "instofclass " 1
  "\\term{instance} of the \\term{class} \\typeref{~A}")
(define-term "instsofclass " 1
  "\\term{instances} of the \\term{class} \\typeref{~A}")
(define-term "Instsofclass " 1
  "\\term{Instances} of the \\term{class} \\typeref{~A}")
(define-term "instanceofclasses " 1
  "\\term{generalized instance} of \\theclass{~A}")
(define-term "instancesofclasses " 1
  "\\term{generalized instances} of \\theclass{~A}")
(define-term "Instancesofclasses " 1
  "\\term{Generalized instances} of \\theclass{~A}")
(define-term "Theloopconstruct " 1
  "The \\macref{loop} \\loopref{~A} construct")
(define-term "theloopconstruct " 1
  "the \\macref{loop} \\loopref{~A} construct")
(define-term "Theloopkeyword " 1
  "The \\macref{loop} \\loopref{~A} keyword")
(define-term "theloopkeyword " 1
  "the \\macref{loop} \\loopref{~A} keyword")
(define-term "thevalueof " 1
  "the \\term{value} of \\misc{~A}")
(define-term "Thevalueof " 1
  "The \\term{value} of \\misc{~A}")
(define-term "thevaluesof " 1
  "the \\term{values} of \\misc{~A}")
(define-term "Thevaluesof " 1
  "The \\term{values} of \\misc{~A}")
(define-term "formatOp" 1
  "{\\dummy}\\hbox{{\\tt ~~~A}}")
(define-term "formatdirective" 1
  "{\\dummy}\\hbox{{\\tt ~~~A}} format directive")
(define-term "NamedTypePredicate" 3
  "\\funref{~A} returns \\term{true} if \\param{~A} is \\oftype{~A}; otherwise, it returns \\term{false}.")
(define-term "TypePredicate" 2
  "Returns \\term{true} if \\param{~A} is \\oftype{~A}; otherwise, returns \\term{false}.")
(define-term "NamedPredicate" 3
  "\\funref{~A} returns \\term{true} if \\param{~A} is ~A; otherwise, returns \\term{false}.")
(define-term "Predicate" 2
  "Returns \\term{true} if \\param{~A} is ~A; otherwise, returns \\term{false}.")
(define-term "StrictPredicate" 2
  "Returns \\misc{t} if \\param{~A} is ~A; otherwise, returns \\misc{nil}.")
(define-term "Shouldcheckplus" 1
  "Should signal an error \\oftype{program-error} if at least one \\param{~A} is not supplied.")
(define-term "Checktype" 2
  "Signals an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "Checktypes" 2
  "Signals an error \\oftype{type-error} if ~A are not ~A.")
(define-term "Checknottype" 2
  "Signals an error \\oftype{type-error} if \\param{~A} is ~A.")
(define-term "Checknottypes" 2
  "Signals an error \\oftype{type-error} if ~A are ~A.")
(define-term "Checkanytype" 2
  "Signals an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "Shouldchecktype" 2
  "Should signal an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "Shouldcheckanytype" 2
  "Should signal an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "Lazychecktype" 2
  "Should be prepared to signal an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "Lazychecktypes" 2
  "Should be prepared to signal an error \\oftype{type-error} if ~A are not ~A.")
(define-term "Lazychecknottype" 2
  "Should be prepared to signal an error \\oftype{type-error} if \\param{~A} is ~A.")
(define-term "Lazycheckanytype" 2
  "Should be prepared to signal an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "Lazycheckanynottype" 2
  "Should be prepared to signal an error \\oftype{type-error} if any \\param{~A} is ~A.")
(define-term "checktype" 2
  "signals an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "checkanytype" 2
  "signals an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "shouldchecktype" 2
  "should signal an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "shouldcheckanytype" 2
  "should signal an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "lazychecktype" 2
  "should be prepared to signal an error \\oftype{type-error} if \\param{~A} is not ~A.")
(define-term "lazycheckanytype" 2
  "should be prepared to signal an error \\oftype{type-error} if any \\param{~A} is not ~A.")
(define-term "Default" 1
  "The default is ~A.")
(define-term "DefaultFor" 2
  "The default for ~A is ~A.")
(define-term "DefaultIn" 2
  "The default in ~A is ~A.")
(define-term "Defaults" 2
  "The defaults for ~A are ~A, respectively.")
(define-term "DefaultEach" 2
  "The defaults for each of ~A is ~A.")
(define-term "DefaultsIn" 3
  "The defaults for ~A in ~A are ~A, respectively.")

;;; Characters

(define-abbrev "NewlineChar"
  "\\ang{Newline}")
(define-abbrev "SpaceChar"
  "\\ang{Space}")
(define-abbrev "TabChar"
  "\\ang{Tab}")
(define-abbrev "ReturnChar"
  "\\ang{Return}")
(define-abbrev "LinefeedChar"
  "\\ang{Linefeed}")
(define-abbrev "BackspaceChar"
  "\\ang{Backspace}")
(define-abbrev "PageChar"
  "\\ang{Page}")
(define-abbrev "RuboutChar"
  "\\ang{Rubout}")
(define-abbrev "WhitespaceChar"
  "\\ang{Whitespace}")

(define-abbrev "bq"
  "`")

;;; Subscripts

(define-abbrev "ssso"
  "\\sub{1}")
(define-abbrev "ssst"
  "\\sub{2}")
(define-abbrev "ssse"
  "\\sub{8}")
(define-abbrev "ssss"
  "\\sub{16}")

(define-abbrev "sssi"
  "\\sub{i}")
(define-abbrev "sssk"
  "\\sub{k}")
(define-abbrev "sssn"
  "\\sub{n}")
(define-abbrev "sssx"
  "\\sub{x}")
(define-abbrev "sssy"
  "\\sub{y}")
(define-abbrev "sssz"
  "\\sub{z}")

;;; Misc

(define-abbrev "symbolnamedesignator"
  "string designator")
(define-abbrev "symbolnamedesignators"
  "string designators")
(define-abbrev "packagenamedesignator"
  "string designator")
(define-abbrev "packagenamedesignators"
  "string designators")

;;; Sections
;;;
;;; These are typically auto generated, but the spec is static, so we can
;;; hardcode them

(define-abbrev "ChapOne"
  "Chapter 1 (Introduction)")
(define-abbrev "ChapTwo"
  "Chapter 2 (Syntax)")
(define-abbrev "ChapThree"
  "Chapter 3 (Evaluation and Compilation)")
(define-abbrev "ChapFour"
  "Chapter 4 (Types and Classes)")
(define-abbrev "ChapFive"
  "Chapter 5 (Data and Control Flow)")
(define-abbrev "ChapSix"
  "Chapter 6 (Iteration)")
(define-abbrev "ChapSeven"
  "Chapter 7 (Objects)")
(define-abbrev "ChapEight"
  "Chapter 8 (Structures)")
(define-abbrev "ChapNine"
  "Chapter 9 (Conditions)")
(define-abbrev "ChapTen"
  "Chapter 10 (Symbols)")
(define-abbrev "ChapEleven"
  "Chapter 11 (Packages)")
(define-abbrev "ChapTwelve"
  "Chapter 12 (Numbers)")
(define-abbrev "ChapThirteen"
  "Chapter 13 (Characters)")
(define-abbrev "ChapFourteen"
  "Chapter 14 (Conses)")
(define-abbrev "ChapFifteen"
  "Chapter 15 (Arrays)")
(define-abbrev "ChapSixteen"
  "Chapter 16 (Strings)")
(define-abbrev "ChapSeventeen"
  "Chapter 17 (Sequences)")
(define-abbrev "ChapEighteen"
  "Chapter 18 (Hash Tables)")
(define-abbrev "ChapNineteen"
  "Chapter 19 (Filenames)")
(define-abbrev "ChapTwenty"
  "Chapter 20 (Files)")
(define-abbrev "ChapTwentyOne"
  "Chapter 21 (Streams)")
(define-abbrev "ChapTwentyTwo"
  "Chapter 22 (Printer)")
(define-abbrev "ChapTwentyThree"
  "Chapter 23 (Reader)")
(define-abbrev "ChapTwentyFour"
  "Chapter 24 (System Construction)")
(define-abbrev "ChapTwentyFive"
  "Chapter 25 (Environment)")
(define-abbrev "ChapTwentySix"
  "Chapter 26 (Glossary)")
(define-abbrev "ChapA"
  "Chapter A (Appendix)")
(define-abbrev "Introduction"
  "Chapter 1 (Introduction)")
(define-abbrev "Definitions"
  "Section 1.4 (Definitions)")
(define-abbrev "ModifiedBNF"
  "Section 1.4.1.2 (Modified BNF Syntax)")
(define-abbrev "CaseInSymbols"
  "Section 1.4.1.4.1 (Case in Symbols)")
(define-abbrev "Designators"
  "Section 1.4.1.5 (Designators)")
(define-abbrev "ErrorTerms"
  "Section 1.4.2 (Error Terminology)")
(define-abbrev "RemovableText"
  "Section 1.4.3 (Sections Not Formally Part Of This Standard)")
(define-abbrev "InterpretingDictionaryEntries"
  "Section 1.4.4 (Interpreting Dictionary Entries)")
(define-abbrev "TypeSpecEntries"
  "Section 1.4.4.6 (Dictionary Entries for Type Specifiers)")
(define-abbrev "Conformance"
  "Section 1.5 (Conformance)")
(define-abbrev "ReqLangFeatures"
  "Section 1.5.1.1 (Required Language Features)")
(define-abbrev "ReadTimeConditionals"
  "Section 1.5.2.1.1 (Use of Read-Time Conditionals)")
(define-abbrev "LanguageExtensions"
  "Section 1.6 (Language Extensions)")
(define-abbrev "LanguageSubsets"
  "Section 1.7 (Language Subsets)")
(define-abbrev "DeprecatedFeatures"
  "Section 1.8 (Deprecated Language Features)")
(define-abbrev "CLsymbols"
  "Section 1.9 (Symbols in the COMMON-LISP Package)")
(define-abbrev "Syntax"
  "Chapter 2 (Syntax)")
(define-abbrev "CharacterSyntax"
  "Section 2.1 (Character Syntax)")
(define-abbrev "TheStandardSyntax"
  "Section 2.1 (Character Syntax)")
(define-abbrev "Readtables"
  "Section 2.1.1 (Readtables)")
(define-abbrev "CurrentReadtable"
  "Section 2.1.1.1 (The Current Readtable)")
(define-abbrev "ReaderVars"
  "Section 2.1.2 (Variables that affect the Lisp Reader)")
(define-abbrev "StandardChars"
  "Section 2.1.3 (Standard Characters)")
(define-abbrev "CharacterSyntaxTypes"
  "Section 2.1.4 (Character Syntax Types)")
(define-abbrev "ConstituentChars"
  "Section 2.1.4.1 (Constituent Characters)")
(define-abbrev "ConstituentTraits"
  "Section 2.1.4.2 (Constituent Traits)")
(define-abbrev "InvalidChars"
  "Section 2.1.4.3 (Invalid Characters)")
(define-abbrev "MacroChars"
  "Section 2.1.4.4 (Macro Characters)")
(define-abbrev "MultipleEscapeChar"
  "Section 2.1.4.5 (Multiple Escape Characters)")
(define-abbrev "SingleEscapeChar"
  "Section 2.1.4.6 (Single Escape Character)")
(define-abbrev "WhitespaceChars"
  "Section 2.1.4.7 (Whitespace Characters)")
(define-abbrev "ReaderAlgorithm"
  "Section 2.2 (Reader Algorithm)")
(define-abbrev "InterpOfTokens"
  "Section 2.3 (Interpretation of Tokens)")
(define-abbrev "PotentialNumbersAsTokens"
  "Section 2.3.1.1 (Potential Numbers as Tokens)")
(define-abbrev "EscCharsAndPotentialNums"
  "Section 2.3.1.1.1 (Escape Characters and Potential Numbers)")
(define-abbrev "NumsFromTokens"
  "Section 2.3.2 (Constructing Numbers from Tokens)")
(define-abbrev "SyntaxOfIntegers"
  "Section 2.3.2.1.1 (Syntax of an Integer)")
(define-abbrev "SyntaxOfRatios"
  "Section 2.3.2.1.2 (Syntax of a Ratio)")
(define-abbrev "SyntaxOfFloats"
  "Section 2.3.2.2 (Syntax of a Float)")
(define-abbrev "SyntaxOfComplexes"
  "Section 2.3.2.3 (Syntax of a Complex)")
(define-abbrev "SymbolTokens"
  "Section 2.3.4 (Symbols as Tokens)")
(define-abbrev "PackageSysConsistencyRules"
  "Section 2.3.6 (Package System Consistency Rules)")
(define-abbrev "StandardMacroChars"
  "Section 2.4 (Standard Macro Characters)")
(define-abbrev "LeftParen"
  "Section 2.4.1 (Left-Parenthesis)")
(define-abbrev "QuoteMacro"
  "Section 2.4.3 (Single-Quote)")
(define-abbrev "Doublequote"
  "Section 2.4.5 (Double-Quote)")
(define-abbrev "Backquote"
  "Section 2.4.6 (Backquote)")
(define-abbrev "SharpsignBackslash"
  "Section 2.4.8.1 (Sharpsign Backslash)")
(define-abbrev "SharpsignQuote"
  "Section 2.4.8.2 (Sharpsign Single-Quote)")
(define-abbrev "SharpsignLeftParen"
  "Section 2.4.8.3 (Sharpsign Left-Parenthesis)")
(define-abbrev "SharpsignStar"
  "Section 2.4.8.4 (Sharpsign Asterisk)")
(define-abbrev "SharpsignColon"
  "Section 2.4.8.5 (Sharpsign Colon)")
(define-abbrev "SharpsignDot"
  "Section 2.4.8.6 (Sharpsign Dot)")
(define-abbrev "SharpsignB"
  "Section 2.4.8.7 (Sharpsign B)")
(define-abbrev "SharpsignO"
  "Section 2.4.8.8 (Sharpsign O)")
(define-abbrev "SharpsignX"
  "Section 2.4.8.9 (Sharpsign X)")
(define-abbrev "SharpsignR"
  "Section 2.4.8.10 (Sharpsign R)")
(define-abbrev "SharpsignC"
  "Section 2.4.8.11 (Sharpsign C)")
(define-abbrev "SharpsignA"
  "Section 2.4.8.12 (Sharpsign A)")
(define-abbrev "SharpsignS"
  "Section 2.4.8.13 (Sharpsign S)")
(define-abbrev "SharpsignP"
  "Section 2.4.8.14 (Sharpsign P)")
(define-abbrev "SharpsignLeftAngle"
  "Section 2.4.8.20 (Sharpsign Less-Than-Sign)")
(define-abbrev "EvaluationAndCompilation"
  "Chapter 3 (Evaluation and Compilation)")
(define-abbrev "Evaluation"
  "Section 3.1 (Evaluation)")
(define-abbrev "IntroToEnvs"
  "Section 3.1.1 (Introduction to Environments)")
(define-abbrev "NullLexicalEnv"
  "Section 3.1.1.3.1 (The Null Lexical Environment)")
(define-abbrev "EnvObjs"
  "Section 3.1.1.4 (Environment Objects)")
(define-abbrev "EvaluationModel"
  "Section 3.1.2 (The Evaluation Model)")
(define-abbrev "SymbolsAsForms"
  "Section 3.1.2.1.1 (Symbols as Forms)")
(define-abbrev "ConstantVars"
  "Section 3.1.2.1.1.3 (Constant Variables)")
(define-abbrev "FunctionForms"
  "Section 3.1.2.1.2.3 (Function Forms)")
(define-abbrev "LambdaForms"
  "Section 3.1.2.1.2.4 (Lambda Forms)")
(define-abbrev "LambdaExpressions"
  "Section 3.1.3 (Lambda Expressions)")
(define-abbrev "Shadowing"
  "Section 3.1.5 (Shadowing)")
(define-abbrev "Compilation"
  "Section 3.2 (Compilation)")
(define-abbrev "CompilationTerms"
  "Section 3.2.1 (Compiler Terminology)")
(define-abbrev "ConstantModification"
  "Section 3.2.1 (Compiler Terminology)")
(define-abbrev "CompilationSemantics"
  "Section 3.2.2 (Compilation Semantics)")
(define-abbrev "CompilerMacros"
  "Section 3.2.2.1 (Compiler Macros)")
(define-abbrev "MinimalCompilation"
  "Section 3.2.2.2 (Minimal Compilation)")
(define-abbrev "SemanticConstraints"
  "Section 3.2.2.3 (Semantic Constraints)")
(define-abbrev "FileCompilation"
  "Section 3.2.3 (File Compilation)")
(define-abbrev "TopLevelForms"
  "Section 3.2.3.1 (Processing of Top Level Forms)")
(define-abbrev "DefiningMacros"
  "Section 3.2.3.1.1 (Processing of Defining Macros)")
(define-abbrev "ConstraintsOnMacros"
  "Section 3.2.3.1.2 (Constraints on Macros and Compiler Macros)")
(define-abbrev "LiteralsInCompiledFiles"
  "Section 3.2.4 (Literal Objects in Compiled Files)")
(define-abbrev "ExternalizableObjects"
  "Section 3.2.4.1 (Externalizable Objects)")
(define-abbrev "Similarity"
  "Section 3.2.4.2 (Similarity of Literal Objects)")
(define-abbrev "CallingMakeLoadForm"
  "Section 3.2.4.4 (Additional Constraints on Externalizable Objects)")
(define-abbrev "FileCompilerExceptions"
  "Section 3.2.5 (Exceptional Situations in the Compiler)")
(define-abbrev "Declarations"
  "Section 3.3 (Declarations)")
(define-abbrev "DeclScope"
  "Section 3.3.4 (Declaration Scope)")
(define-abbrev "LambdaLists"
  "Section 3.4 (Lambda Lists)")
(define-abbrev "OrdinaryLambdaLists"
  "Section 3.4.1 (Ordinary Lambda Lists)")
(define-abbrev "SuppressingKeyArgChecks"
  "Section 3.4.1.4.1 (Suppressing Keyword Argument Checking)")
(define-abbrev "GFLambdaLists"
  "Section 3.4.2 (Generic Function Lambda Lists)")
(define-abbrev "SpecializedLambdaLists"
  "Section 3.4.3 (Specialized Lambda Lists)")
(define-abbrev "MacroLambdaLists"
  "Section 3.4.4 (Macro Lambda Lists)")
(define-abbrev "ExtraDestructureInfo"
  "Section 3.4.4 (Macro Lambda Lists)")
(define-abbrev "DestructuringByLambdaLists"
  "Section 3.4.4.1 (Destructuring by Lambda Lists)")
(define-abbrev "DestructuringLambdaLists"
  "Section 3.4.5 (Destructuring Lambda Lists)")
(define-abbrev "BoaLambdaLists"
  "Section 3.4.6 (Boa Lambda Lists)")
(define-abbrev "DefsetfLambdaLists"
  "Section 3.4.7 (Defsetf Lambda Lists)")
(define-abbrev "DeftypeLambdaLists"
  "Section 3.4.8 (Deftype Lambda Lists)")
(define-abbrev "DefineModifyMacroLambdaLists"
  "Section 3.4.9 (Define-modify-macro Lambda Lists)")
(define-abbrev "DefMethCombArgsLambdaLists"
  "Section 3.4.10 (Define-method-combination Arguments Lambda Lists)")
(define-abbrev "DocVsDecls"
  "Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)")
(define-abbrev "FuncallErrorChecking"
  "Section 3.5 (Error Checking in Function Calls)")
(define-abbrev "SafeAndUnsafeCalls"
  "Section 3.5.1.1 (Safe and Unsafe Calls)")
(define-abbrev "SafeCallDetectionTime"
  "Section 3.5.1.1.1 (Error Detection Time in Safe Calls)")
(define-abbrev "UnrecognizedKeyArgs"
  "Section 3.5.1.4 (Unrecognized Keyword Arguments)")
(define-abbrev "InvalidKeyArgs"
  "Section 3.5.1.5 (Invalid Keyword Arguments)")
(define-abbrev "OddNumberOfKeyArgs"
  "Section 3.5.1.6 (Odd Number of Keyword Arguments)")
(define-abbrev "DestructuringMismatch"
  "Section 3.5.1.7 (Destructuring Mismatch)")
(define-abbrev "TraversalRules"
  "Section 3.6 (Traversal Rules and Side Effects)")
(define-abbrev "DestructiveOperations"
  "Section 3.7 (Destructive Operations)")
(define-abbrev "TypesAndClasses"
  "Chapter 4 (Types and Classes)")
(define-abbrev "Types"
  "Section 4.2 (Types)")
(define-abbrev "TypeRelationships"
  "Section 4.2.2 (Type Relationships)")
(define-abbrev "TypeSpecifiers"
  "Section 4.2.3 (Type Specifiers)")
(define-abbrev "Classes"
  "Section 4.3 (Classes)")
(define-abbrev "Inheritance"
  "Section 4.3.4 (Inheritance)")
(define-abbrev "DeterminingtheCPL"
  "Section 4.3.5 (Determining the Class Precedence List)")
(define-abbrev "ClassReDef"
  "Section 4.3.6 (Redefining Classes)")
(define-abbrev "IntegratingTypesAndClasses"
  "Section 4.3.7 (Integrating Types and Classes)")
(define-abbrev "DataAndControlFlow"
  "Chapter 5 (Data and Control Flow)")
(define-abbrev "GeneralizedReference"
  "Section 5.1 (Generalized Reference)")
(define-abbrev "GenRefSubFormEval"
  "Section 5.1.1.1 (Evaluation of Subforms to Places)")
(define-abbrev "SetfExpansions"
  "Section 5.1.1.2 (Setf Expansions)")
(define-abbrev "KindsOfPlaces"
  "Section 5.1.2 (Kinds of Places)")
(define-abbrev "FnFormsAsGenRefs"
  "Section 5.1.2.2 (Function Call Forms as Places)")
(define-abbrev "SETFofVALUES"
  "Section 5.1.2.3 (VALUES Forms as Places)")
(define-abbrev "SETFofAPPLY"
  "Section 5.1.2.5 (APPLY Forms as Places)")
(define-abbrev "TransferOfControl"
  "Section 5.2 (Transfer of Control to an Exit Point)")
(define-abbrev "Iteration"
  "Chapter 6 (Iteration)")
(define-abbrev "LoopFacility"
  "Section 6.1 (The LOOP Facility)")
(define-abbrev "SimpleLoop"
  "Section 6.1.1.1.1 (Simple Loop)")
(define-abbrev "DestructuringLOOPVars"
  "Section 6.1.1.7 (Destructuring)")
(define-abbrev "LOOPVarInitAndStep"
  "Section 6.1.2 (Variable Initialization and Stepping Clauses)")
(define-abbrev "LOOPValAcc"
  "Section 6.1.3 (Value Accumulation Clauses)")
(define-abbrev "LOOPTermTest"
  "Section 6.1.4 (Termination Test Clauses)")
(define-abbrev "LOOPUnconditional"
  "Section 6.1.5 (Unconditional Execution Clauses)")
(define-abbrev "LOOPConditional"
  "Section 6.1.6 (Conditional Execution Clauses)")
(define-abbrev "LOOPMisc"
  "Section 6.1.7 (Miscellaneous Clauses)")
(define-abbrev "Objects"
  "Chapter 7 (Objects)")
(define-abbrev "ObjectCreationAndInit"
  "Section 7.1 (Object Creation and Initialization)")
(define-abbrev "DeclaringInitargValidity"
  "Section 7.1.2 (Declaring the Validity of Initialization Arguments)")
(define-abbrev "InitargRules"
  "Section 7.1.4 (Rules for Initialization Arguments)")
(define-abbrev "SharedInitialize"
  "Section 7.1.5 (Shared-Initialize)")
(define-abbrev "ChangingInstanceClass"
  "Section 7.2 (Changing the Class of an Instance)")
(define-abbrev "InitNewLocalSlots"
  "Section 7.2.2 (Initializing Newly Added Local Slots)")
(define-abbrev "InstanceReInit"
  "Section 7.3 (Reinitializing an Instance)")
(define-abbrev "MetaObjects"
  "Section 7.4 (Meta-Objects)")
(define-abbrev "Slots"
  "Section 7.5 (Slots)")
(define-abbrev "SlotInheritance"
  "Section 7.5.3 (Inheritance of Slots and Slot Options)")
(define-abbrev "GFsAndMethods"
  "Section 7.6 (Generic Functions and Methods)")
(define-abbrev "IntroToGFs"
  "Section 7.6.1 (Introduction to Generic Functions)")
(define-abbrev "IntroToMethods"
  "Section 7.6.2 (Introduction to Methods)")
(define-abbrev "SpecializerQualifierAgreement"
  "Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers)")
(define-abbrev "GFMethodLambdaListCongruency"
  "Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function)")
(define-abbrev "KwdArgsInGFsAndMeths"
  "Section 7.6.5 (Keyword Arguments in Generic Functions and Methods)")
(define-abbrev "MethodSelectionAndCombination"
  "Section 7.6.6 (Method Selection and Combination)")
(define-abbrev "DeterminingtheEffectiveMethod"
  "Section 7.6.6.1 (Determining the Effective Method)")
(define-abbrev "SelApplMeth"
  "Section 7.6.6.1.1 (Selecting the Applicable Methods)")
(define-abbrev "ApplyMethCombToSortedMethods"
  "Section 7.6.6.1.3 (Applying method combination to the sorted list of applicable methods)")
(define-abbrev "StdMethComb"
  "Section 7.6.6.2 (Standard Method Combination)")
(define-abbrev "BuiltInMethCombTypes"
  "Section 7.6.6.4 (Built-in Method Combination Types)")
(define-abbrev "MethodInheritance"
  "Section 7.6.7 (Inheritance of Methods)")
(define-abbrev "Structures"
  "Chapter 8 (Structures)")
(define-abbrev "Conditions"
  "Chapter 9 (Conditions)")
(define-abbrev "ConditionSystemConcepts"
  "Section 9.1 (Condition System Concepts)")
(define-abbrev "ConditionDesignators"
  "Section 9.1.2.1 (Condition Designators)")
(define-abbrev "PrintingConditions"
  "Section 9.1.3 (Printing Conditions)")
(define-abbrev "CondSignalHandle"
  "Section 9.1.4 (Signaling and Handling Conditions)")
(define-abbrev "Signaling"
  "Section 9.1.4.1 (Signaling)")
(define-abbrev "Restarts"
  "Section 9.1.4.2 (Restarts)")
(define-abbrev "InterfacesToRestarts"
  "Section 9.1.4.2.2 (Interfaces to Restarts)")
(define-abbrev "AssocRestartWithCond"
  "Section 9.1.4.2.4 (Associating a Restart with a Condition)")
(define-abbrev "Symbols"
  "Chapter 10 (Symbols)")
(define-abbrev "Packages"
  "Chapter 11 (Packages)")
(define-abbrev "PackageConcepts"
  "Section 11.1 (Package Concepts)")
(define-abbrev "Numbers"
  "Chapter 12 (Numbers)")
(define-abbrev "NumericOperations"
  "Section 12.1.1 (Numeric Operations)")
(define-abbrev "NumericContagionRules"
  "Section 12.1.1.2 (Contagion in Numeric Operations)")
(define-abbrev "RationalComputations"
  "Section 12.1.3 (Rational Computations)")
(define-abbrev "FloatSubstitutability"
  "Section 12.1.3.3 (Rule of Float Substitutability)")
(define-abbrev "FloatingPointComputations"
  "Section 12.1.4 (Floating-point Computations)")
(define-abbrev "RuleOfFloatAndRationalContagion"
  "Section 12.1.4.1 (Rule of Float and Rational Contagion)")
(define-abbrev "RuleOfFloatPrecisionContagion"
  "Section 12.1.4.4 (Rule of Float Precision Contagion)")
(define-abbrev "ComplexComputations"
  "Section 12.1.5 (Complex Computations)")
(define-abbrev "RuleOfComplexContagion"
  "Section 12.1.5.2 (Rule of Complex Contagion)")
(define-abbrev "RuleOfCanonRepForComplexRationals"
  "Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals)")
(define-abbrev "IntervalDesignators"
  "Section 12.1.6 (Interval Designators)")
(define-abbrev "Characters"
  "Chapter 13 (Characters)")
(define-abbrev "CharacterConcepts"
  "Section 13.1 (Character Concepts)")
(define-abbrev "IntroToChars"
  "Section 13.1.1 (Introduction to Characters)")
(define-abbrev "CharScripts"
  "Section 13.1.2.1 (Character Scripts)")
(define-abbrev "CharRepertoires"
  "Section 13.1.2.2 (Character Repertoires)")
(define-abbrev "CharacterAttributes"
  "Section 13.1.3 (Character Attributes)")
(define-abbrev "GraphicChars"
  "Section 13.1.4.1 (Graphic Characters)")
(define-abbrev "CharactersWithCase"
  "Section 13.1.4.3 (Characters With Case)")
(define-abbrev "Digits"
  "Section 13.1.4.6 (Digits in a Radix)")
(define-abbrev "CharacterNames"
  "Section 13.1.7 (Character Names)")
(define-abbrev "TreatmentOfNewline"
  "Section 13.1.8 (Treatment of Newline during Input and Output)")
(define-abbrev "CharEncodings"
  "Section 13.1.9 (Character Encodings)")
(define-abbrev "ImplementationDefinedScripts"
  "Section 13.1.10 (Documentation of Implementation-Defined Scripts)")
(define-abbrev "Conses"
  "Chapter 14 (Conses)")
(define-abbrev "Arrays"
  "Chapter 15 (Arrays)")
(define-abbrev "ArrayConcepts"
  "Section 15.1 (Array Concepts)")
(define-abbrev "ArrayElements"
  "Section 15.1.1 (Array Elements)")
(define-abbrev "ArrayUpgrading"
  "Section 15.1.2.1 (Array Upgrading)")
(define-abbrev "RequiredSpecializedArrays"
  "Section 15.1.2.2 (Required Kinds of Specialized Arrays)")
(define-abbrev "Strings"
  "Chapter 16 (Strings)")
(define-abbrev "StringConcepts"
  "Section 16.1 (String Concepts)")
(define-abbrev "StringsAreArrays"
  "Section 16.1.1 (Implications of Strings Being Arrays)")
(define-abbrev "Sequences"
  "Chapter 17 (Sequences)")
(define-abbrev "SequenceConcepts"
  "Section 17.1 (Sequence Concepts)")
(define-abbrev "TestFunctionRules"
  "Section 17.2 (Rules about Test Functions)")
(define-abbrev "SatisfyingTheTwoArgTest"
  "Section 17.2.1 (Satisfying a Two-Argument Test)")
(define-abbrev "SatisfyingTheOneArgTest"
  "Section 17.2.2 (Satisfying a One-Argument Test)")
(define-abbrev "HashTables"
  "Chapter 18 (Hash Tables)")
(define-abbrev "HashTableConcepts"
  "Section 18.1 (Hash Table Concepts)")
(define-abbrev "ModifyingHashKeys"
  "Section 18.1.2 (Modifying Hash Table Keys)")
(define-abbrev "VisModEQ"
  "Section 18.1.2.1 (Visible Modification of Objects with respect to EQ and EQL)")
(define-abbrev "VisModEQL"
  "Section 18.1.2.1 (Visible Modification of Objects with respect to EQ and EQL)")
(define-abbrev "VisModEQUAL"
  "Section 18.1.2.2 (Visible Modification of Objects with respect to EQUAL)")
(define-abbrev "Filenames"
  "Chapter 19 (Filenames)")
(define-abbrev "NamingFiles"
  "Section 19.1 (Overview of Filenames)")
(define-abbrev "PathnamesAsFilenames"
  "Section 19.1.2 (Pathnames as Filenames)")
(define-abbrev "PathnameConcepts"
  "Section 19.2 (Pathnames)")
(define-abbrev "PathnameComponents"
  "Section 19.2.1 (Pathname Components)")
(define-abbrev "PathnameComponentCase"
  "Section 19.2.2.1.2 (Case in Pathname Components)")
(define-abbrev "SpecialComponentValues"
  "Section 19.2.2.2 (Special Pathname Component Values)")
(define-abbrev "WildComponents"
  "Section 19.2.2.2.2 (:WILD as a Component Value)")
(define-abbrev "UnspecificComponent"
  "Section 19.2.2.2.3 (:UNSPECIFIC as a Component Value)")
(define-abbrev "WildcardRestrictions"
  "Section 19.2.2.3 (Restrictions on Wildcard Pathnames)")
(define-abbrev "ConstructingPathnames"
  "Section 19.2.2.5 (Restrictions on Constructing Pathnames)")
(define-abbrev "MergingPathnames"
  "Section 19.2.3 (Merging Pathnames)")
(define-abbrev "LogicalPathnames"
  "Section 19.3 (Logical Pathnames)")
(define-abbrev "LogPathNamestrings"
  "Section 19.3.1 (Syntax of Logical Pathname Namestrings)")
(define-abbrev "LogicalPathCompUnspecific"
  "Section 19.3.2.1 (Unspecific Components of a Logical Pathname)")
(define-abbrev "Files"
  "Chapter 20 (Files)")
(define-abbrev "FileSystemConcepts"
  "Section 20.1 (File System Concepts)")
(define-abbrev "StreamsToPathnames"
  "Section 20.1.1 (Coercion of Streams to Pathnames)")
(define-abbrev "OpenAndClosedStreams"
  "Section 20.1.2 (File Operations on Open and Closed Streams)")
(define-abbrev "Truenames"
  "Section 20.1.3 (Truenames)")
(define-abbrev "Streams"
  "Chapter 21 (Streams)")
(define-abbrev "StreamConcepts"
  "Section 21.1 (Stream Concepts)")
(define-abbrev "OpenAndClosedStreams"
  "Section 21.1.1.1.2 (Open and Closed Streams)")
(define-abbrev "InteractiveStreams"
  "Section 21.1.1.1.3 (Interactive Streams)")
(define-abbrev "StreamArgsToStandardizedFns"
  "Section 21.1.3 (Stream Arguments to Standardized Functions)")
(define-abbrev "Printer"
  "Chapter 22 (Printer)")
(define-abbrev "TheLispPrinter"
  "Section 22.1 (The Lisp Printer)")
(define-abbrev "PrinterDispatch"
  "Section 22.1.2 (Printer Dispatching)")
(define-abbrev "DefaultPrintObjMeths"
  "Section 22.1.3 (Default Print-Object Methods)")
(define-abbrev "PrintingIntegers"
  "Section 22.1.3.1.1 (Printing Integers)")
(define-abbrev "PrintingRatios"
  "Section 22.1.3.1.2 (Printing Ratios)")
(define-abbrev "PrintingFloats"
  "Section 22.1.3.1.3 (Printing Floats)")
(define-abbrev "PrintingComplexes"
  "Section 22.1.3.1.4 (Printing Complexes)")
(define-abbrev "PrintingCharacters"
  "Section 22.1.3.2 (Printing Characters)")
(define-abbrev "PrintingSymbols"
  "Section 22.1.3.3 (Printing Symbols)")
(define-abbrev "ReadtableCasePrintEffect"
  "Section 22.1.3.3.2 (Effect of Readtable Case on the Lisp Printer)")
(define-abbrev "ReadtableCasePrintExamples"
  "Section 22.1.3.3.2.1 (Examples of Effect of Readtable Case on the Lisp Printer)")
(define-abbrev "PrintingStrings"
  "Section 22.1.3.4 (Printing Strings)")
(define-abbrev "PrintingListsAndConses"
  "Section 22.1.3.5 (Printing Lists and Conses)")
(define-abbrev "PrintingBitVectors"
  "Section 22.1.3.6 (Printing Bit Vectors)")
(define-abbrev "PrintingOtherVectors"
  "Section 22.1.3.7 (Printing Other Vectors)")
(define-abbrev "PrintingOtherArrays"
  "Section 22.1.3.8 (Printing Other Arrays)")
(define-abbrev "PrintingRandomStates"
  "Section 22.1.3.10 (Printing Random States)")
(define-abbrev "PrintingPathnames"
  "Section 22.1.3.11 (Printing Pathnames)")
(define-abbrev "PrintingStructures"
  "Section 22.1.3.12 (Printing Structures)")
(define-abbrev "PrintingOtherObjects"
  "Section 22.1.3.13 (Printing Other Objects)")
(define-abbrev "PPrinter"
  "Section 22.2 (The Lisp Pretty Printer)")
(define-abbrev "DynamicControlofOutput"
  "Section 22.2.1.1 (Dynamic Control of the Arrangement of Output)")
(define-abbrev "CompilingFormatStrings"
  "Section 22.2.1.3 (Compiling Format Strings)")
(define-abbrev "PPrintDispatchTables"
  "Section 22.2.1.4 (Pretty Print Dispatch Tables)")
(define-abbrev "PrettyPrinterExamples"
  "Section 22.2.2 (Examples of using the Pretty Printer)")
(define-abbrev "FormattedOutput"
  "Section 22.3 (Formatted Output)")
(define-abbrev "FORMATPrinterOps"
  "Section 22.3.4 (FORMAT Printer Operations)")
(define-abbrev "TildeUnderscore"
  "Section 22.3.5.1 (Tilde Underscore: Conditional Newline)")
(define-abbrev "TildeLessThanLogicalBlock"
  "Section 22.3.5.2 (Tilde Less-Than-Sign: Logical Block)")
(define-abbrev "TildeI"
  "Section 22.3.5.3 (Tilde I: Indent)")
(define-abbrev "TildeLessThanJustification"
  "Section 22.3.6.2 (Tilde Less-Than-Sign: Justification)")
(define-abbrev "Reader"
  "Chapter 23 (Reader)")
(define-abbrev "ReaderConcepts"
  "Section 23.1 (Reader Concepts)")
(define-abbrev "ReadtableCaseReadEffect"
  "Section 23.1.2 (Effect of Readtable Case on the Lisp Reader)")
(define-abbrev "ReadtableCaseReadExamples"
  "Section 23.1.2.1 (Examples of Effect of Readtable Case on the Lisp Reader)")
(define-abbrev "SystemConstruction"
  "Chapter 24 (System Construction)")
(define-abbrev "Features"
  "Section 24.1.2 (Features)")
(define-abbrev "FeatureExpressions"
  "Section 24.1.2.1 (Feature Expressions)")
(define-abbrev "FeatureExpExamples"
  "Section 24.1.2.1.1 (Examples of Feature Expressions)")
(define-abbrev "Environment"
  "Chapter 25 (Environment)")
(define-abbrev "TopLevelLoop"
  "Section 25.1.1 (Top level loop)")
(define-abbrev "Time"
  "Section 25.1.4 (Time)")
(define-abbrev "DecodedTime"
  "Section 25.1.4.1 (Decoded Time)")
(define-abbrev "UniversalTime"
  "Section 25.1.4.2 (Universal Time)")
(define-abbrev "InternalTime"
  "Section 25.1.4.3 (Internal Time)")
(define-abbrev "Glossary"
  "Chapter 26 (Glossary)")
(define-abbrev "Appendix"
  "Chapter A (Appendix)")
(define-abbrev "RemovedFeatures"
  "Section A.1 (Removed Language Features)")
