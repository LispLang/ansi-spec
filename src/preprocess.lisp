;;;; Process TeX comments, \input tags, etc.

(in-package :cl-ansi-spec)

(defparameter *filters* (list))

(defmacro define-filter (regex replacement)
  "Define a filter that replaces `regex` with `replacement`."
  `(push
    (lambda (string)
      (cl-ppcre:regex-replace-all ,regex
                                  string
                                  ,replacement
                                  :simple-calls t))
    *filters*))

(defmacro define-tag-filter (tag-name replacement)
  `(define-filter (format nil "\\\\~A" ,tag-name) ,replacement))

(defun filter (string)
  (let ((filtered-string string))
    (loop for filter in (reverse *filters*) do
      (setf filtered-string (funcall filter filtered-string)))
    filtered-string))

;;; Chapters

(defparameter +begin-chapter-regexp+
  "\\\\beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}")

(defparameter +begin-chapter-fmt+
  "\\chapter[index='~A', title='~A', chap-id='~A', ref-title='~A']{")

(define-filter "\\\\beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}"
    (lambda (match &rest regs)
      (declare (ignore match))
      (format nil +begin-chapter-fmt+
              (first regs)
              (second regs)
              (third regs)
              (fourth regs))))

(define-tag-filter "endchapter" "}")

;;; Sections

(define-filter "\\\\beginSection{([^}]+)}[^\\\\]*\\\\DefineSection{([^}]+)}"
    (lambda (match &rest regs)
      (declare (ignore match))
      (format nil "\\section[title='~A', ref='~A']{"
              (first regs)
              (second regs))))

(define-filter "\\\\beginSection{([^}]+)}"
    (lambda (match &rest regs)
      (declare (ignore match))
      (format nil "\\section[title='~A']{" (first regs))))

(define-tag-filter "endSection" "}")

;;; Sub-sections

(define-filter "\\\\beginsubSection{([^}]+)}[^\\\\]*\\\\DefineSection{([^}]+)}"
    (lambda (match &rest regs)
      (declare (ignore match))
      (format nil "\\subsection[title='~A', ref='~A']{"
              (first regs)
              (second regs))))

(define-filter "\\\\beginsubSection{([^}]+)}"
    (lambda (match &rest regs)
      (declare (ignore match))
      (format nil "\\subsection[title='~A']{" (first regs))))

(define-tag-filter "endsubSection" "}")

;;; Lists

(define-tag-filter "beginlist" "\\list{")
(define-tag-filter "endlist" "}")

(define-tag-filter "bull" "\\bullet{}")

;;; References

(define-tag-filter "seesection\\\\(\\w+)"
  (lambda (match &rest regs)
    (declare (ignore match))
    (format nil "\\seesection{~A}" (first regs))))

(define-tag-filter "seefigure\\\\(\\w+)"
  (lambda (match &rest regs)
    (declare (ignore match))
    (format nil "\\seefigure{~A}" (first regs))))

(define-tag-filter "seechapter\\\\(\\w+)"
  (lambda (match &rest regs)
    (declare (ignore match))
    (format nil "\\seechapter{~A}" (first regs))))

;;; Quotes

(define-filter "``" "\\doublequotes{")
(define-filter "''" "}")

;;; Simple removals

(define-tag-filter "input setup" "")
(define-tag-filter "%-*- Mode: TeX -*-" "")
(define-tag-filter "bye" "")

;;; Comments

(define-filter "([^\\\\])%.*"
    (lambda (match &rest regs)
      (declare (ignore match))
      (first regs)))

;;; Include files

(defun include-inputs (string)
  "Replace all instances of '\input file-name' with the contents of 'file-name.tex'."
  (labels ((valid-input-p (file-name)
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

(defun preprocess (string)
  (filter
   (expand-abbreviations
    (include-inputs string))))
