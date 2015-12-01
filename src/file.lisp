(in-package :cl-user)
(defpackage ansi-spec.file
  (:use :cl)
  (:export :+tex-directory+
           :+chapter-files+
           :+output-file+
           :with-output-file)
  (:documentation "Files and directories used by this library."))
(in-package :ansi-spec.file)

(defparameter +tex-directory+
  (asdf:system-relative-pathname :ansi-spec #p"tex/")
  "The directory where the TeX sources of the spec are stored.")

(defparameter +chapter-files+
  (sort (directory (merge-pathnames #p"chap-*.tex" +tex-directory+))
        #'<
        :key #'(lambda (pathname)
                 (let ((string (remove-if #'(lambda (char)
                                              (not (digit-char-p char)))
                                          (pathname-name pathname))))
                   (if (string= string "")
                       100000000000
                       (parse-integer string)))))
  "List of pathnames of the chapter files.")

(defparameter +output-directory+
  (asdf:system-relative-pathname :ansi-spec #p"spec/"))

(defparameter +output-file+
  (merge-pathnames #p"output.xml" +output-directory+))

(defmacro with-output-file ((stream) &body body)
  `(progn
     (ensure-directories-exist +output-directory+)
     (with-open-file (,stream +output-file+
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
       ,@body)))
