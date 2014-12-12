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
         (full-content (preprocess content)))
    (make-instance '<chapter>
                   :pathname pathname
                   :content full-content)))
