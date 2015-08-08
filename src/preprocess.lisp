(in-package :cl-user)
(defpackage ansi-spec.preprocess
  (:use :cl)
  (:export :preprocess)
  (:documentation "Prepare TeX input for parsing."))
(in-package :ansi-spec.preprocess)

(defun include-inputs (text)
  "Replace all instances of '\input file-name' with the contents of
'file-name.tex'."
  (labels ((valid-input-p (file-name)
             (and (not (search "fig" file-name))
                  (not (search ".tc" file-name))
                  (not (equal file-name "index.idx"))))
           (include-file (name)
             (let ((input-pathname
                     (make-pathname :name name
                                    :type "tex"
                                    :defaults ansi-spec.file:+tex-directory+)))
               ;; Ignore figures
               (if (valid-input-p name)
                   (progn
                     (format t "~&  Including path '~A.~A'"
                             (pathname-name input-pathname)
                             (pathname-type input-pathname))
                     (uiop:read-file-string input-pathname))
                   ""))))
    (cl-ppcre:regex-replace-all "\\input ([^ \\n]+)\\n"
                                text
                                #'(lambda (match &rest regs)
                                    (declare (ignore match))
                                    (include-file (first regs)))
                                :simple-calls t)))

(defun strip-unwanted (text)
  "Remove TeX we don't want."
  (let ((regexes (list "([^\\\\]|^)%.*" ;; comments
                       "\\\\input.*" ;; the input directive
                       ))
        (output text))
    (loop for regex in regexes do
      (setf output (ppcre:regex-replace-all regex output "")))
    output))

(defun ampersand-directive (text)
  "Make the ampersand character into a directive."
  (ppcre:regex-replace-all "([^\\\\])&" text "\\1\\ampersand"))

(defparameter +chapter-format+
"beginchapter{} \\beginchapterindex{~A} \\beginchaptertitle{~A} \\beginchapterid{~A} \\beginchapterreftitle{~A}")

(defun simpler-chapter-definition (text)
  "The \\beginchapter directive has four bodies. We move some of those to attributes."
  (ppcre:regex-replace-all "beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}"
                           text
                           (lambda (match &rest regs)
                             (declare (ignore match))
                             (format nil +chapter-format+
                                     (first regs)
                                     (second regs)
                                     (third regs)
                                     (fourth regs)))
                           :simple-calls t))

(defun preprocess (text)
  (simpler-chapter-definition
   (ampersand-directive
    (strip-unwanted
     (include-inputs text)))))
