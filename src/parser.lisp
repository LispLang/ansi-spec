(in-package :cl-ansi-spec)

(defclass <chapter> ()
  ((pathname :reader chapter-pathname
             :initarg :pathname
             :type pathname)
   (content :reader chapter-content
            :initarg :content
            :type string)
   (node :accessor chapter-node
         :type plump:node)))

(defmethod initialize-instance :after ((chapter <chapter>) &key)
  (setf (chapter-node chapter)
        (plump-tex:parse (chapter-content chapter))))

(defun parse-chapter (pathname)
  (log:info "Parsing chapter ~S" pathname)
  (let* ((content (uiop:read-file-string pathname))
         (full-content (remove-comments (include-inputs content))))
    (make-instance '<chapter>
                   :pathname pathname
                   :content full-content)))
