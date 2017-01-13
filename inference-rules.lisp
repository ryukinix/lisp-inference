;; Common Lisp Script
;; Manoel Vilela

;; inference rules for propositional calculus

(load "parser.lisp")
(load "operators.lisp")

(defun modus-ponens (exp)
  "Modus Ponens inference rule ::
  (^ (=> p q) p) => q"
  (if (and (conjunctionp exp)
           (implicationp (first-operand exp))
           (eq (first-of-first-operand exp )
               (second-operand exp)))
      (second-of-first-operand exp)
      exp))

(defun modus-tollens (exp)
  "Modus Tollens inference rule ::
   (^ (=> p q) (~ p)) => (~ q)"
  (if (and (conjunctionp exp)
           (implicationp (first-operand exp))
           (or (equalp (make-negation (first-of-first-operand exp))
                       (second-operand exp))
               (equalp (first-of-first-operand exp)
                       (make-negation (second-operand exp)))))
      (make-negation (second-operand (first-operand exp)))
      exp))

(defun syllogism-disjunctive (exp)
  "Syllogism Disjunctive inference rule ::
   (^ (v p q) (~ p)) => q"
  (if (and (conjunctionp exp)
           (disjunctionp (first-operand exp))
           (negationp (second-operand exp))
           (find-if #'(lambda (x)
                        (equalp x (first-of-first-operand exp)))
                    (operands (first-operand exp))))
      (car (remove-if #'(lambda (x)
                          (equalp x (first-of-second-operand exp)))
                      (operands (first-operand exp))))
      exp))

(defun addiction (exp p)
  "Addiction in inference rule ::
   p => (v p q)"
  (make-disjunction exp p))

(defun conjunction (exp p)
  "Conjunction inference rule ::
   p => (^ p q)"
  (make-conjunction exp p))

(defun simplification-generic (exp operand)
  "When exp is a conjunction select operand
   (^ p q) => p
   (^ p q) => q
   operand :: 'first-operand, 'second-operand"
  (if (conjunctionp exp)
      (funcall operand exp)
      exp))

(defun simplification-first (exp)
  "Simplification with first operand rule ::
  (^ p q) => p"
  (simplification-generic exp 'first-operand))

(defun simplification-second (exp)
  "Simplification with second operand rule ::
  (^ p q) => q"
  (simplification-generic exp 'second-operand))

(defun syllogism-hypothetical (exp)
  "Syllogism Hypothetical inference rule ::
  (^ (=> x y) (=> y z)) => (=> x z)
  (^ (=> y z) (=> x y)) => (=> x z)"
  (if (and (conjunctionp exp)
           (implicationp (first-operand exp))
           (implicationp (second-operand exp))
           (or (equalp (second-of-first-operand exp)
                       (first-of-second-operand exp))
               (equalp (first-of-first-operand exp)
                       (second-of-second-operand exp))))
      (if (equalp (second-of-first-operand exp)
                  (first-of-second-operand exp))
          (make-implication
                (first-of-first-operand exp)
                (second-of-second-operand exp))
          (make-implication
                (first-of-second-operand exp)
                (second-of-first-operand exp)))
      exp))

(defun absorption (exp)
  "Absorption inference rule ::
  (=> p q) => (=> p (^ p q))"
  (if (implicationp exp)
      (make-implication (first-operand exp)
                        (make-conjunction (first-operand exp)
                                          (second-operand exp)))))
