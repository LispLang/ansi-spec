(in-package :cl-user)
(defpackage cl-ansi-spec
  (:use :cl))
(in-package :cl-ansi-spec)

(defparameter +tex-directory+
  (asdf:system-relative-pathname :cl-ansi-spec #p"tex/")
  "The directory where the TeX sources of the spec are stored.")

(defparameter +chapter-files+
  (directory (merge-pathnames #p"chap-*.tex" +tex-directory+))
  "List of pathnames of the chapter files.")

(defun parse-chapters ()
  (mapcar #'(lambda (chap) (plump-tex:parse chap))
          +chapter-files+))
