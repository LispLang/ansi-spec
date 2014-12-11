(in-package :cl-ansi-spec)

(defclass <chapter> ()
  ((pathname :reader chapter-pathname
             :initarg :pathname
             :type pathname)
   (content :reader chapter-content
            :initarg :content
            :type string)))

(defun parse-chapter (pathname)
  (log:info "Parsing chapter ~S" pathname)
  (let* ((content (uiop:read-file-string pathname))
         (full-content (include-inputs content)))
    (make-instance '<chapter>
                   :pathname pathname
                   :content full-content)))
