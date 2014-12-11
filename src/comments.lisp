(in-package :cl-ansi-spec)

(defun remove-comments (string)
  "Remove TeX comments from a string."
  (let ((lines (split-sequence:split-sequence #\Newline string)))
    (reduce #'(lambda (l r)
                (concatenate 'string l (string #\Newline) r))
            (remove-if #'(lambda (line)
                           (and (> (length line) 0)
                                (char= (elt line 0) #\%)))
                       lines))))
