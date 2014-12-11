(in-package :cl-ansi-spec)

(defun ensure-output-directory ()
  (log:info "Ensuring output directory exists")
  (ensure-directories-exist +output-directory+))

(defun generate-spec ()
  "Parse the spec into JSON files from sources."
  (ensure-output-directory)
  (let ((chapter (parse-chapter (third +chapter-files+))))
    (print (chapter-content chapter)))
  t
  ;(loop for chapter-pathname in +chapter-files+ do
  ;  (let* ((chapter (parse-chapter chapter-pathname))
  ;         (filtered (tex2xml (chapter-node chapter))))
  ;    (print (chapter-content chapter))
  ;    t)))
  )
