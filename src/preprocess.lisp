;;;; Process TeX comments, \input tags, etc.

(in-package :cl-ansi-spec)

;;; Regex-based filtering

(defparameter *filters* (list))

(defmacro define-filter (regex replacement)
  "Define a filter that replaces `regex` with `replacement`."
  `(push
    (lambda (string)
      (cl-ppcre:regex-replace-all ,regex
                                  string
                                  ,replacement))
    *filters*))

(defmacro define-tag-filter (tag-name replacement)
  `(define-filter (format nil "\\\\~A" ,tag-name) ,replacement))

(defun filter (string)
  (let ((filtered-string string))
    (loop for filter in *filters* do
      (setf filtered-string (funcall filter filtered-string)))
    filtered-string))

;; Quotes
(define-filter "``" "\\quote{")
(define-filter "''" "}")

;; Simple removals
(define-filter "\\\\input setup" "")
(define-filter "\\\\%-*- Mode: TeX -*-" "")
(define-tag-filter "bye" "")

;;; More complex filtering

(defun include-inputs (string)
  "Replace all instances of '\input file-name' with the contents of 'file-name.tex'."
  (flet ((valid-input-p (file-name)
           (and (not (search "fig" file-name))
                (not (search ".tc" file-name))
                (not (equal file-name "index.idx"))))
         (include-file (name)
           (let ((input-pathname
                   (make-pathname :name name
                                  :type "tex"
                                  :defaults +tex-directory+)))
             ;; Ignore figures
             (if (valid-input-p name)
                 (progn
                   (log:info "Including path ~S" input-pathname)
                   (uiop:read-file-string input-pathname))
                 ""))))
    (cl-ppcre:regex-replace-all "\\input ([^ \\n]+)\\n"
                                string
                                #'(lambda (match &rest regs)
                                    (declare (ignore match))
                                    (include-file (first regs)))
                                :simple-calls t)))

(defun remove-comments (string)
  "Remove TeX comments from a string."
  (cl-ppcre:regex-replace-all "([^\\\\])%.*"
                              string
                              #'(lambda (match &rest regs)
                                  (declare (ignore match))
                                  (first regs))
                              :simple-calls t))

;;; Simplify the \beginChapter thing

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

(defun preprocess (string)
  (filter
   (simplify-begin-chapter
    (expand-abbreviations
     (remove-comments
      (include-inputs string))))))
