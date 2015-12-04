;;;; setup-tables.tex
(in-package :ansi-spec.traverse)

;;; Tables
;;;
;;; Each 'display{two|three|four|five}' macro takes two arguments: A title and
;;; the contents of the table. In the table contents, rows are separated by
;;; '\cr' tags, and columns in each row are separated by ampersands (which were
;;; converted to the '\ampersand' directive in a previus transform).

(loop for column-count in (list "two" "three" "four" "five") do
  (define-mode ((concatenate 'string "display" column-count))
    :callbacks
    ((()
      ;; Called on the `display{nth}` element, whose body is the title
      (output (format nil "~%<table>~%<title>~%")))
     (()
      (output (format nil "~%</title>~%<body>~%<row>~%<cell>"))))))

(loop for column-count in (list "two" "three" "four" "five") do
  (define-mode ((concatenate 'string "show" column-count))
    :callbacks
    ((()
      ;; Called on the `display{nth}` element, whose body is the title
      (output (format nil "~%<table class=\"borderless\">~%<title>~%")))
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
