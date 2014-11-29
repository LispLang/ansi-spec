(in-package :cl-user)
(defpackage cl-ansi-spec
  (:use :cl)
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

;;; Utility functions

(defun ensure-output-directory ()
  (log:info "Ensuring output directory exists")
  (ensure-directories-exist +output-directory+))

(defparameter +input-tag-regexp+
  "\\input ([^ \\n]+)\\n")

(defun valid-input-p (file-name)
  (and (not (search "fig" file-name))
       (not (search ".tc" file-name))
       (not (equal file-name "index.idx"))))

(defun include-file (name)
  (let ((input-pathname
          (make-pathname :name name
                         :type "tex"
                         :defaults +tex-directory+)))
    ;; Ignore figures
    (if (valid-input-p name)
        (progn
          (log:info "Including path ~S" input-pathname)
          (uiop:read-file-string input-pathname))
        "")))

(defun include-inputs (string)
  "Replace all instances of '\input file-name' with the contents of 'file-name.tex'."
  (cl-ppcre:regex-replace-all +input-tag-regexp+
                              string
                              #'(lambda (match &rest regs)
                                  (include-file (first regs)))
                              :simple-calls t))

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

(defun generate-spec ()
  "Parse the spec into JSON files from sources."
  (ensure-output-directory)
  (loop for chapter-pathname in +chapter-files+ do
    (parse-chapter chapter-pathname)))
