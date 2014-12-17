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

(define-tag-filter "beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}"
  (lambda (match &rest regs)
    (declare (ignore match))
    (format nil "\\chapter[index='~A', title='~A', chap-id='~A', ref-title='~A']{"
            (first regs)
            (second regs)
            (third regs)
            (fourth regs))))

(define-tag-filter "endchapter" "}")

;;; Issues

(define-tag-filter "issue{([^}]+)}"
  (lambda (match &rest regs)
    (declare (ignore match))
    (format nil "\\issue[name='~A']{" (first regs))))

(define-tag-filter "endissue" "}")

;;; Sections

(defmacro define-section-filter (sub-level)
  `(progn
     (define-filter (concatenate 'string
                                 "\\\\begin"
                                 ,sub-level
                                 "[Ss]ection{([^}]+)}[^\\\\]*\\\\DefineSection{([^}]+)}")
         (lambda (match &rest regs)
           (declare (ignore match))
           (let ((title (first regs))
                 (ref (second regs)))
             (format nil (concatenate 'string
                                      "\\"
                                      ,sub-level
                                      "section[title='~A', ref='~A']{")
                     title ref))))

     (define-filter (concatenate 'string
                                 "\\\\begin"
                                 ,sub-level
                                 "[Ss]ection{([^}]+)}")
         (lambda (match &rest regs)
           (declare (ignore match))
           (format nil (concatenate 'string
                                    "\\"
                                    ,sub-level
                                    "section[title='~A']{")
                   (first regs))))

     (define-tag-filter (concatenate 'string
                                     "end"
                                     ,sub-level
                                     "[Ss]ection")
       "}")))

(define-section-filter "")
(define-section-filter "sub")
(define-section-filter "subsub")
(define-section-filter "subsubsub")
(define-section-filter "subsubsubsub")

;;; Lists

(define-tag-filter "beginlist" "\\list{")
(define-tag-filter "endlist" "}")

(define-tag-filter "bull" "\\bullet{}")

;;; Tables

(defun create-table (title body)
  (let* ((lines (cl-ppcre:split "\\\\cr\\n" body))
         (content (loop for line in lines collecting
                    (cl-ppcre:split "&" line))))
    (format nil "\\table[title='~A']{~{\\row{~{\\cell{~A}~}}~}}"
            title
            content)))

(defmacro define-table-filter (column-count)
  `(define-tag-filter (concatenate 'string
                                   "display"
                                   ,column-count
                                   "{([^}]+)}{([^}]+)}")
       (lambda (match &rest regs)
         (declare (ignore match))
         (create-table (first regs) (second regs)))))

(define-table-filter "two")
(define-table-filter "three")
(define-table-filter "four")
(define-table-filter "five")

;;; Quotes

(define-filter "``" "\\doublequotes{")
(define-filter "''" "}")

;;; Code

(define-tag-filter "code" "\\code{")
(define-tag-filter "endcode" "}")

;;; Simple removals

(defmacro define-null-tag (regexp)
  `(define-tag-filter ,regexp ""))

(define-null-tag "input setup")
(define-null-tag "\\% -\\*- Mode: TeX -\\*-")
(define-null-tag "\\%-\\*- Mode: TeX -\\*-")
(define-null-tag "bye")
(define-null-tag "vfill")
(define-null-tag "eject")
(define-null-tag "vtop")
(define-null-tag "hbox")
(define-null-tag "cr")
(define-null-tag "Vskip")
(define-null-tag "hfil")

;;; Comments

(define-filter "([^\\\\])%.*"
  (lambda (match &rest regs)
    (declare (ignore match))
    (first regs)))

;;; Document-related Shorthand

(defmacro define-doc-filter (regexp arg &rest body)
  `(define-tag-filter ,(format nil "[~A~A]~A{([^}]+)}"
                               (char-upcase (elt regexp 0))
                               (char-downcase (elt regexp 0))
                               (subseq regexp 1))
       (lambda (match &rest regs)
         (declare (ignore match))
         (let ((,arg (first regs)))
           ,@body))))

(define-doc-filter "seefun" ref
  (format nil "See the \\term{function} \\funref{~A}" ref))

(define-doc-filter "seefuns" ref
  (format nil "See the \\term{functions} \\funref{~A}" ref))

(define-doc-filter "seespec" ref
  (format nil "See the \\term{special operator} \\specref{~A}" ref))

(define-doc-filter "seemac" ref
  (format nil "See the \\term{macro} \\macref{~A}" ref))

(define-doc-filter "seevar" ref
  (format nil "See the \\term{variable} \\varref{~A}" ref))

(define-doc-filter "seetype" ref
  (format nil "See the \\term{type} \\typeref{~A}" ref))

(define-doc-filter "seemisc" ref
  (format nil "See \\miscref{~A}" ref))

(define-doc-filter "seesection" ref
  (format nil "See \\secref{~A}" ref))

(define-doc-filter "seechapter" ref
  (format nil "See \\chapref{~A}" ref))

(define-doc-filter "seefigure" ref
  (format nil "See \\figref{~A}" ref))

(define-doc-filter "seeterm" ref
  (format nil "See \\term{~A}" ref))

(define-doc-filter "seetermAlso" ref
  (format nil "See also \\term{~A}" ref))

;;; Include files

(defun explicit-bodies (string)
  (log:info "Ensuring all TeX commands have explicit bodies")
  (let ((regex "\\\\(\\w+)\\\\(\\w+)"))
    (ppcre:regex-replace-all regex
                             string
                             (lambda (match &rest regs)
                               (declare (ignore match))
                               (let ((op (first regs))
                                     (body (second regs)))
                                 (format nil "\\~A{~A}" op body)))
                             :simple-calls t)))

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
  (log:info "Preprocessing")
  (filter
   (expand-abbreviations
    (explicit-bodies
     (include-inputs string)))))
