(in-package :cl-user)
(defpackage ansi-spec-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :ansi-spec-test)

(def-suite tests)
(in-suite tests)

(test explicit-body
  (is
   (equal (let ((node (plump:parse "<a><div><node/>a b c</div></a>")))
            (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
          "<a><node>a b c</node></a>"))
  (is
   (equal (let ((node (plump:parse "<div><node/>a b c</div>")))
            (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
          "<node>a b c</node>"))
  (is
   (equal
    (let ((node (plump-tex:parse "{\\node a b c}")))
      (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
    "<node> a b c</node>")))

(test generate
  (finishes
    (ansi-spec:generate)))

(defun run-tests ()
  (run! 'tests))
