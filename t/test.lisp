(defpackage #:lisp-inference/test
  (:use #:cl
        #:lisp-inference
        #:rove))

(in-package :lisp-inference/test)


(deftest inference-rules-test
  (testing "== Inference rules!"
    (ok (equal (modus-ponens '(^ (=> p q) p))
               'q)
        "Inference: MODUS-PONENS")

    (ok (equal (modus-tollens '(^ (=> p q) (~ p)))
               '(~ q))
        "Inference: MODUS-TOLLENS")

    (ok (equal (syllogism-disjunctive '(^ (v p q) (~ p)))
               'q)
        "Inference: SYLLOGISM-DISJUNCTIVE")

    (ok (equal (syllogism-hypothetical '(^ (=> x y) (=> y z)))
               '(=> X Z))
        "Inference: SYLLOGISM-HYPOTHETICAL")

    (ok (equal (addiction 'p 'q)
               '(v p q))
        "Inference: ADDICTION")

    (ok (equal (conjunction '(=> p q) 'p)
               '(^ (=> P Q) P))
        "Inference: CONJUNCTION")

    (ok (equal (absorption '(=> r (^ x y)))
               '(=> R (^ R (^ X Y))))
        "Inference: ABSORPTION")

    (ok (equal (simplification-first '(^ p q))
               'p)
        "Inference: SIMPLIFICATION FIRST")

    (ok (equal (simplification-second '(^ r s))
               's)
        "Inference: SIMPLIFICATION SECOND")))


(deftest equivalence-rules-test
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

(deftest infix-parsing-test
  (testing "== Infix Parsing"
    (ok (equal (infix-to-prefix '(~ (p v q)))
               '(~ (v p q))))

    (ok (equal (infix-to-prefix '(p => q))
               '(=> p q)))

    (ok (equal (infix-to-prefix '((p v q) <=> ((~ p) ^ (~ q))))
               '(<=> (v p q)
                    (^ (~ p)
                       (~ q)))))))
