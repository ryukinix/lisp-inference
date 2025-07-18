(defpackage #:lisp-inference/tests/test-infix-parsing
  (:use #:cl
        #:lisp-inference
        #:rove))

(in-package #:lisp-inference/tests/test-infix-parsing)

;; deprecated in favor of pratt parser
(deftest test-infix-parsing
  (testing "== Infix Parsing"
    (ok (equal (infix-to-prefix '(~ (p v q)))
               '(~ (v p q))))

    (ok (equal (infix-to-prefix '(p => q))
               '(=> p q)))

    (ok (equal (infix-to-prefix '((p v q) <=> ((~ p) ^ (~ q))))
               '(<=> (v p q)
                    (^ (~ p)
                       (~ q)))))))
