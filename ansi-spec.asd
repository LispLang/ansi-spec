(defsystem ansi-spec
  :version "0.1"
  :author "Fernando Borretti"
  :homepage "https://github.com/eudoxia0/cl-ansi-spec"
  :license "MIT"
  :depends-on (:plump-tex
               :uiop
               :cl-ppcre
               :split-sequence
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "file")
                 (:file "preprocess")
                 (:file "explicit-body")
                 (:file "traverse")
                 (:module "modes"
                  :serial t
                  :components
                  ((:file "macros")
                   (:file "sections")
                   (:file "setup-document")
                   (:file "setup-figures")
                   (:file "setup-title")
                   (:file "setup-version")
                   (:file "others")))
                 (:file "main"))))
  :description "The ANSI Common Lisp draft specification, parsed from TeX sources."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ansi-spec-test))))
