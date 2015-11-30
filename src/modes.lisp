;;;; Define parser modes
(in-package :ansi-spec.traverse)

(define-mode ("div" node :arity 0)
  t)

(define-mode ("bye" node :arity 0)
  ;; For some reason tex documents end with this crap
  nil)
