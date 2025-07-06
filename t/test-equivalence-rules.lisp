(in-package #:lisp-inference/tests/test-equivalence-rules)

(deftest test-equivalence-rules
  (testing "== Equivalence rules!"
    (ok (equal (de-morgan '(^ p q))
               '(~ (v (~ p) (~ q))))
        "Equivalence: DE-MORGAN 1")
    (ok (equal (de-morgan '(~ (v p q)))
               '(^ (~ p) (~ q)))
        "Equivalence: DE-MORGAN 2")

    (ok (equal (de-morgan '(~ (^ (~ p) (~ q))))
               '(v p q))
        "Equivalence: DE-MORGAN 3")

    (ok (equal (double-negation '(~ (~ p)))
               'p)
        "Equivalence: DOUBLE-NEGATION 1")

    (ok (equal (double-negation 'p)
               'p)
        "Equivalence: DOUBLE-NEGATION 2")))
