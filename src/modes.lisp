;;;; Define parser modes
(in-package :ansi-spec.traverse)

(defmacro define-trivial-mode (tag-name xml-tag)
  `(define-mode (,tag-name)
     :callbacks
     ((()
       (output (format nil "<~A>" ,xml-tag))))
     :after
     (()
      (output (format nil "</~A>" ,xml-tag)))))

;;; Formatting

(define-trivial-mode "b" "b")
(define-trivial-mode "i" "i")
(define-trivial-mode "j" "i") ;; Spec says this is 'italic + kerning'. What.
(define-trivial-mode "f" "c") ;; f as in fixed-width, as in monospace, as in
                              ;; code.

;;; Tables

(loop for column-count in (list "two" "three" "four" "five") do
  ;; Each 'display{two|three|four|five}' macro takes two arguments: A title and
  ;; the contents of the table. In the table contents, rows are separated by
  ;; '\cr' tags, and columns in each row are separated by ampersands (which were
  ;; converted to the '\ampersand' directive in a previus transform).
  (define-mode ((concatenate 'string "display" column-count))
    :callbacks
    ((()
      ;; Called on the `display{nth}` element, whose body is the title
      (output (format nil "~%<table>~%<title>~%")))
     (()
      (output (format nil "~%</title>~%<body>~%<row>~%<cell>"))))))

(define-mode ("cr")
  ;; Row separator
  :callbacks
  ((()
    (output (format nil "</cell>~%</row>~%<row>~%<cell>~%")))))

(define-mode ("ampersand")
  ;; Column separator
  :callbacks
  ((()
    (output (format nil "~%</cell>~%<cell>~%")))))

(define-mode ("param")
  ;; A parameter to an operator
  :callbacks
  ((()
    (output "<param>")))
  :after
  (()
   (output "</param>")))

;;; Meaningless no-ops

(mapcar #'(lambda (op)
            (define-mode (op)))
        (list "div"
              "bye"
              "vfill"
              "eject"))
