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

(test sibling-extraction
  (let* ((node (plump:parse "<root><a/><b/><c/></root>"))
         (a (elt (plump:children (elt (plump:children node) 0)) 0)))
    (is
     (equal (plump:tag-name a) "a"))
    (let ((siblings (ansi-spec.traverse::next-n-siblings a 1)))
      (is
       (equal (length siblings) 1))
      (is
       (equal (plump:tag-name (first siblings)) "b")))
    (let ((siblings (ansi-spec.traverse::next-n-siblings a 2)))
      (is
       (equal (length siblings) 2))
      (is
       (equal (plump:tag-name (first siblings)) "b"))
      (is
       (equal (plump:tag-name (second siblings)) "c")))))

(test traversal
  (is
   (equal
    (ansi-spec.traverse:traverse-string "``text''")
    "“text”"))
  (is
   (equal
    (ansi-spec.traverse:traverse-string "\\code{}a b c\\endcode{}")
    "<code>a b c</code>")))

(test generate
  (finishes
    (ansi-spec:generate)))

(defun run-tests ()
  (run! 'tests))
