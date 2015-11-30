(defsystem ansi-spec-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "ansi-spec tests."
  :depends-on (:ansi-spec
               :plump-tex
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "tests"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
