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
                   (output (format nil "\">~%<row><cell>")))))))

(loop for column-count in (list "two" "three" "four" "five") do
  (let ((column-count column-count))
    (define-mode ((concatenate 'string "show" column-count))
                 :callbacks
                 ((()
                   ;; Called on the `display{nth}` element, whose body is the title
                   (output (format nil "~%<table count=~S class=\"borderless\" title=\"" column-count)))
                  (()
                   (output (format nil "\">~%<row><cell>")))))))

(defmacro tablefig (type colnum)
  (let ((headers (loop for i from 1 to (- colnum 1)
                       collect '(() (output (format nil "</cell>~%<cell>")))
                       )))
  `(define-mode ((concatenate 'string "tablefig" ,type))
                :callbacks
                ((()
                  (output (format nil "~%<table count=~S class=\"borderless\" title=\"" ,type)))
                 (()
                  (output (format nil "\">~%<row type=\"header\">~%<cell>")))
                 ,@headers
                 (()
                  (output (format nil "</cell></row>~%<row><cell>")))
                 ))))

(tablefig "two" 2)
(tablefig "three" 3)
(tablefig "four" 4)
(tablefig "five" 5)
(tablefig "six" 6)

(loop for mode in (list "cr" "ampersand") do
  (let ((mode mode))
    (define-mode (mode)
                 ;; Row separator
                 :callbacks
                 (((node)
                   (output (format nil "</cell>"))
                   (when (string= mode "cr") (output (format nil "</row>")))
                   (if (plump:next-element node)
                       (progn
                         (when (string= mode "cr") (output (format nil "<row>")))
                         (output (format nil "~%<cell>")))
                       (output (format nil "</table>"))
                       ))))))
