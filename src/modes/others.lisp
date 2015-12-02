(in-package :ansi-spec.traverse)

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

(define-alias "goodbreak" "

")

(define-trivial-mode "sub" "sub")

;;; Macro definition

(define-mode ("def")
  ;; Just destroy everything
  :callbacks
  (((node)
    (let ((nodes (siblings-until
                  node
                  #'(lambda (node)
                      (and (plump:element-p node)
                           (string= (plump:tag-name node) "div"))))))
      (loop for def-node across nodes do
        (plump:remove-child def-node))))))

;;; Meaningless no-ops

(define-mode ("pageno")
  ;; Adsorb another
  :callbacks
  (((node)
    (let ((next (plump:next-sibling node)))
      (plump:remove-child next)))))

(mapcar #'(lambda (op)
            (define-mode (op)))
        (list "div"
              "bye"
              "vfill"
              "eject"
              "hbox"
              "noalign"
              "vskip"
              "Vskip"
              "noindent"
              "hfil"
              "hrule"
              "bull"
              "halign"
              "hsize"
              "iskip"
              "leftskip"))
