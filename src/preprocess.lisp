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

(defun strip-comments (text)
  "Remove TeX comments from a string of text."
  (ppcre:regex-replace-all "([^\\\\])%.*"
                           text
                           #'(lambda (match &rest regs)
                               (declare (ignore match))
                               (first regs))
                           :simple-calls t))

(defun ampersand-directive (text)
  "Make the ampersand character into a directive."
  (ppcre:regex-replace-all "[^\\\\]&" text "\\ampersand"))

(defun simpler-chapter-definition (text)
  "The \\beginchapter directive has four bodies. We move some of those to attributes."
  (ppcre:regex-replace-all "beginchapter{([^}]+)}{([^}]+)}{([^}]+)}{([^}]+)}"
                           text
                           (lambda (match &rest regs)
                             (declare (ignore match))
                             (format nil "\\beginchapter[index='~A', title='~A', chap-id='~A', ref-title='~A']{"
                                     (first regs)
                                     (second regs)
                                     (third regs)
                                     (fourth regs)))))

(defun preprocess (text)
  (simpler-chapter-definition
   (ampersand-directive
    (strip-comments
     (include-inputs text)))))
