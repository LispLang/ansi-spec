(in-package :ansi-spec.traverse)

(defmacro define-string-mode (tag-name before-string after-string)
  `(define-mode (,tag-name)
     :callbacks
     ((()
       (output ,before-string)))
     :after
     (()
      (output ,after-string))))

(defmacro define-trivial-mode (tag-name xml-tag)
  `(define-string-mode ,tag-name
     (format nil "<~A>" ,xml-tag)
     (format nil "</~A>" ,xml-tag)))

(defmacro define-alias (tag alias)
  `(define-string-more ,tag-name
     ""
     ,alias))
