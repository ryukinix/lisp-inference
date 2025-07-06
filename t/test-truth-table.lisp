(in-package :lisp-inference/tests/test-truth-table)

(deftest truth-table-tests
  (testing "== Truth-table tests!"
    (ok (equal (eval-expression '(^ p q))
               "TFFF")
        "AND OPERATION: p ^ q")

    (ok (equal (eval-expression '(v p q))
               "TTTF")
        "OR OPERATION: p v q")

    (ok (equal (eval-expression '(=> p q))
               "TFTT")
        "CONDITIONAL OPERATION: p => q")

    (ok (equal (eval-expression '(<=> p q))
               "TFFT")
        "BICONDITIONAL OPERATION: p <=> q")

    (ok (equal (eval-expression '([+] p q))
               "FTTF")
        "XOR OPERATION: p [+] q")

    (ok (equal (eval-expression '(~ p))
               "FT")
        "NOT OPERATION: ~ p")

    (ok (equal-expression '(^ p q)
                          (de-morgan '(^ p q)))
        "EQUAL EXPRESSION 1")

    (ok (equal-expression '(~ (~ p))
                          'p)
        "EQUAL EXPRESSION 2")))
