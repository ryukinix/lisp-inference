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

(deftest truth-table-tests-with-false-and-true
  (testing "== Truth-table tests with F and T as constant!"
    (ok (equal (eval-expression '(^ p f))
               "FF")
        "CONTRADICTION: p ^ f")

    (ok (equal (eval-expression '(v p t))
               "TT")
        "TAUTOLOGY: p v t")

    (ok (equal (eval-expression (parse-logic "(~p v q <=> p => q) <=> t"))
               "TTTT")
        "TAUTOLOGY OF CONDITIONAL DEFINITION")))

(deftest truth-table-tests-with-too-many-vars
    (ok (signals (let ((*max-propositions* 2))
                   (print-truth-table '(^ (=> p q) r)))
               'simple-error)
        "Raise a error when there is more propositions than specified at *MAX-PROPOSITIONS*"))


(deftest truth-table-without-duplicated-stack-of-expressions
  (ok (equal (inference::stack-of-expressions '(^ (~ p) (~ p)))
             '((~ p) (^ (~ p) (~ p))))))
