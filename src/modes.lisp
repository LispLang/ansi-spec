;;;; Define parser modes
(in-package :ansi-spec.traverse)
#|
(define-mode ("div" node pos :arity 0)
  (declare (ignore node pos))
  nil)

(loop for column-count in (list "two" "three" "four" "five") do
  ;; Each 'display{two|three|four|five}' macro takes two arguments: A title and
  ;; the contents of the table. In the table contents, rows are separated by
  ;; '\cr' tags, and columns in each row are separated by ampersands (which were
  ;; converted to the '\ampersand' directive in a previus transform).
  (define-mode ((concatenate 'string "display" column-count) node pos :arity 2)
    (print node)
    (cond
      ((= pos 0)
       ;; Node with the title
       (output (format nil "<table>~%"))
       (output "<title>"))
      ((= pos 1)
       (output (format nil "~%</title>~%"))
       (output "<body>")
       (output "</body>")))))

(define-mode ("bye" node pos :arity 0)
  ;; For some reason tex documents end with this crap
  (declare (ignore node pos))
  nil)
|#
