;;;; Define parser modes
(in-package :ansi-spec.traverse)

(define-mode ("div"))

(loop for column-count in (list "two" "three" "four" "five") do
  ;; Each 'display{two|three|four|five}' macro takes two arguments: A title and
  ;; the contents of the table. In the table contents, rows are separated by
  ;; '\cr' tags, and columns in each row are separated by ampersands (which were
  ;; converted to the '\ampersand' directive in a previus transform).
  (define-mode ((concatenate 'string "display" column-count))
    #'(lambda (node)
        ;; Called on the title
        (print node))))

(define-mode ("bye")
  ;; For some reason tex documents end with this crap
  )
