(in-package :cl-user)
(defpackage cl-ansi-spec-asd
  (:use :cl :asdf))
(in-package :cl-ansi-spec-asd)

(defsystem cl-ansi-spec
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:plump-tex
               :uiop
               :cl-ppcre
               :log4cl
               :split-sequence
               :anaphora)
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "abbreviations")
                 (:file "preprocess")
                 (:file "transform")
                 (:file "interface"))))
  :description "The ANSI Common Lisp draft specification, parsed from TeX sources."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
