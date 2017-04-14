;;;; setup-tables.tex
(in-package :ansi-spec.traverse)

;;; Tables
;;;
;;; Each 'display{two|three|four|five}' macro takes two arguments: A title and
;;; the contents of the table. In the table contents, rows are separated by
;;; '\cr' tags, and columns in each row are separated by ampersands (which were
;;; converted to the '\ampersand' directive in a previus transform).

(loop for column-count in (list "two" "three" "four" "five") do
  (let ((column-count column-count))
    (define-mode ((concatenate 'string "display" column-count))
                 :callbacks
                 ((()
                   ;; Called on the `display{nth}` element, whose body is the title
                   (output (format nil "~%<table count=~S title=\"" column-count)))
                  (()
                   (output (format nil "\">~%<cell>")))))))

(loop for column-count in (list "two" "three" "four" "five") do
  (let ((column-count column-count))
    (define-mode ((concatenate 'string "show" column-count))
                 :callbacks
                 ((()
                   ;; Called on the `display{nth}` element, whose body is the title
                   (output (format nil "~%<table count=~S class=\"borderless\" title=\"" column-count)))
                  (()
                   (output (format nil "\">~%<cell>")))))))


(loop for mode in (list "cr" "ampersand") do
  (define-mode (mode)
               ;; Row separator
               :callbacks
               (((node)
                 (output (format nil "</cell>"))
                 (if (plump:next-element node)
                     (output (format nil "~%<cell>"))
                     (output (format nil "</table>"))
                     )))))
