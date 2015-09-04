(in-package :cl-user)
(defpackage ansi-spec-test
  (:use :cl :prove))
(in-package :ansi-spec-test)

(plan nil)

(is (let ((node (plump:parse "<a><div><node/>a b c</div></a>")))
      (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
    "<a><node>a b c</node></a>")

(is (let ((node (plump:parse "<div><node/>a b c</div>")))
      (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
    "<node>a b c</node>")

(is (let ((node (plump-tex:parse "{\\node a b c}")))
      (plump:serialize (ansi-spec.explicit-body:explicit-body node) nil))
    "<node> a b c</node>")

(finalize)
