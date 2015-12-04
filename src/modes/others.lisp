(in-package :ansi-spec.traverse)

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

(define-alias "lbr" "[")

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
