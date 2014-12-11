;;;; Process TeX comments, \input tags, etc.

(in-package :cl-ansi-spec)

(defparameter +input-tag-regexp+ "\\input ([^ \\n]+)\\n"
  "The regular expression used for the \input tag.")

(defun valid-input-p (file-name)
  (and (not (search "fig" file-name))
       (not (search ".tc" file-name))
       (not (equal file-name "index.idx"))))

(defun include-file (name)
  "Load an included file."
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
                                  (declare (ignore match))
                                  (include-file (first regs)))
                              :simple-calls t))

(defparameter +comment-regexp+ "([^\\\\])%.*")

(defun remove-comments (string)
  "Remove TeX comments from a string."
  (cl-ppcre:regex-replace-all +comment-regexp+
                              string
                              #'(lambda (match &rest regs)
                                  (declare (ignore match))
                                  (first regs))
                              :simple-calls t))

(defun replace-quotes (string)
  "Replace ``TeX quotes`` with <quote> tags."
  (cl-ppcre:regex-replace-all "''"
                              (cl-ppcre:regex-replace-all "``" string "\\quote{")
                              "}"))

(defparameter +begin-chapter-regexp+
  "\\\\beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}")

(defparameter +begin-chapter-fmt+
  "\\chapter[number='~A', title='~A', chap-id='~A', ref-title='~A']{")

(defun simplify-begin-chapter (string)
  (cl-ppcre:regex-replace-all +begin-chapter-regexp+
                              string
                              #'(lambda (match &rest regs)
                                  (declare (ignore match))
                                  (format nil +begin-chapter-fmt+
                                          (first regs)
                                          (second regs)
                                          (third regs)
                                          (fourth regs)))
                              :simple-calls t))

(defun remove-misc (string)
  (cl-ppcre:regex-replace-all
   "\\\\input setup"
   (cl-ppcre:regex-replace-all "\\\\%-*- Mode: TeX -*-"
                               string
                               "")
   ""))

(defun preprocess (string)
  (remove-misc
   (expand-abbreviations
    (simplify-begin-chapter
     (replace-quotes
      (remove-comments
       (include-inputs string)))))))
