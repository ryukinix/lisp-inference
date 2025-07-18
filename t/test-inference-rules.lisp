(defpackage #:lisp-inference/tests/test-inference-rules
  (:use #:cl
        #:lisp-inference
        #:rove))

(in-package #:lisp-inference/tests/test-inference-rules)


(deftest test-inference-rules
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

    (ok (equal (addition 'p 'q)
               '(v p q))
        "Inference: ADDITION")

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
