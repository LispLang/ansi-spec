(in-package :cl-ansi-spec)

(defun ensure-output-directory ()
  (log:info "Ensuring output directory exists")
  (ensure-directories-exist +output-directory+))

(defun generate-spec ()
  "Parse the spec into JSON files from sources."
  (ensure-output-directory)
  (loop for chapter-pathname in (list (third +chapter-files+)) do
    (let* ((chapter (parse-chapter chapter-pathname))
           (filtered (tex->lisp (chapter-node chapter))))
      (print filtered))))
