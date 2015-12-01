(in-package :cl-user)
(defpackage ansi-spec
  (:use :cl)
  (:export :generate)
  (:documentation "The main interface."))
(in-package :ansi-spec)

(defun generate ()
  "Generate the spec."
  (when (probe-file ansi-spec.file:+output-file+)
    (delete-file ansi-spec.file:+output-file+))
  (loop for chapter in ansi-spec.file:+chapter-files+ do
    (ansi-spec.traverse:traverse chapter)))
