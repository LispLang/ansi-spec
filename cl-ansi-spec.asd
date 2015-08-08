(defsystem cl-ansi-spec
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
                :components
                ((:file "file")
                 (:file "preprocess")
                 (:file "traverse")
                 (:file "main"))))
  :description "The ANSI Common Lisp draft specification, parsed from TeX sources."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
