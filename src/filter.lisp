(in-package :cl-ansi-spec)

(defparameter *filters* (list))

(defmacro define-simple-filter (tag-name replacement)
  "A simple search->replace for TeX elements."
  `(push
    (lambda (string)
      (cl-ppcre:regex-replace-all (format nil "\\\\~A" ,tag-name)
                                  string
                                  ,replacement))
    *filters*))

(defmacro define-null-filter (tag-name)
  "A filter that erases everything it matches."
  `(define-simple-filter ,tag-name ""))

(define-null-filter "bye")
(define-null-filter "endchapter")
(define-null-filter "endSection")

(defun filter (string)
  (let ((filtered-string string))
    (loop for filter in *filters* do
      (setf filtered-string (funcall filter filtered-string)))
    filtered-string))
