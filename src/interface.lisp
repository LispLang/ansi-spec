(in-package :cl-ansi-spec)

(defun ensure-output-directory ()
  (log:info "Ensuring output directory exists")
  (ensure-directories-exist +output-directory+))

(defun generate-spec ()
  "Parse the spec into JSON files from sources."
  (ensure-output-directory)
  (format t "~A" (preprocess (uiop:read-file-string (third +chapter-files+))))
  ;(loop for chapter-pathname in +chapter-files+ do
  ;  (let* ((chapter (parse-chapter chapter-pathname))
  ;         (filtered (tex2xml (chapter-node chapter))))
  ;    (print (chapter-content chapter))
  ;    t)))
  )
