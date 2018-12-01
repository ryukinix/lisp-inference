(in-package :lisp-inference/test)


(plan nil)

;; inference rule tests
(diag "== Inference rules!")
(is (modus-ponens '(^ (=> p q) p))
    'q
    "Inference: MODUS-PONENS")
(is (modus-tollens '(^ (=> p q) (~ p)))
    '(~ q)
    "Inference: MODUS-TOLLENS")
(is (syllogism-disjunctive '(^ (v p q) (~ p)))
    'q
    "Inference: SYLLOGISM-DISJUNCTIVE")
(is (syllogism-hypothetical '(^ (=> x y) (=> y z)))
    '(=> X Z)
    "Inference: SYLLOGISM-HYPOTHETICAL")
(is (addiction 'p 'q)
    '(v p q)
    "Inference: ADDICTION")
(is (conjunction '(=> p q) 'p)
    '(^ (=> P Q) P)
    "Inference: CONJUNCTION")
(is (absorption '(=> r (^ x y)))
    '(=> R (^ R (^ X Y)))
    "Inference: ABSORPTION")
(is (simplification-first '(^ p q))
    'p
    "Inference: SIMPLIFICATION FIRST")
(is (simplification-second '(^ r s))
    's
    "Inference: SIMPLIFICATION SECOND")

(diag "== Equivalence rules!")


(is (de-morgan '(^ p q))
    '(~ (v (~ p) (~ q)))
    "Equivalence: DE-MORGAN 1")
(is (de-morgan '(~ (v p q)))
    '(^ (~ p) (~ q))
    "Equivalence: DE-MORGAN 2")
(is (de-morgan '(~ (^ (~ p) (~ q))))
    '(v p q)
    "Equivalence: DE-MORGAN 3")

(is (double-negation '(~ (~ p)))
    'p
    "Equivalence: DOUBLE-NEGATION 1")

(is (double-negation 'p)
    'p
    "Equivalence: DOUBLE-NEGATION 2")


(diag "== Truth-table tests!")

(is (eval-expression '(^ p q))
    "TFFF"
    "AND OPERATION: p ^ q")

(is (eval-expression '(v p q))
    "TTTF"
    "OR OPERATION: p v q")

(is (eval-expression '(=> p q))
    "TFTT"
    "CONDITIONAL OPERATION: p => q")

(is (eval-expression '(<=> p q))
    "TFFT"
    "BICONDITIONAL OPERATION: p <=> q")

(is (eval-expression '([+] p q))
    "FTTF"
    "XOR OPERATION: p [+] q")

(is (eval-expression '(~ p))
    "FT"
    "NOT OPERATION: ~ p")

(finalize)
