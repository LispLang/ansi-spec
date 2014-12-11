(in-package :cl-user)
(defpackage cl-ansi-spec
  (:use :cl :anaphora)
  (:export :generate-spec))
(in-package :cl-ansi-spec)

;;; Constants

(defparameter +tex-directory+
  (asdf:system-relative-pathname :cl-ansi-spec #p"tex/")
  "The directory where the TeX sources of the spec are stored.")

(defparameter +chapter-files+
  (directory (merge-pathnames #p"chap-*.tex" +tex-directory+))
  "List of pathnames of the chapter files.")

(defparameter +output-directory+
  (asdf:system-relative-pathname :cl-ansi-spec #p"spec/"))
