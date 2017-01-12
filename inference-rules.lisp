;; Common Lisp Script
;; Manoel Vilela

;; inference rules for propositional calculus

(load "parser.lisp")

(defun modus-ponens (exp)
  "Modus Ponens inference rule ::
  (^ (=> p q) p) => q"
  (if (and (conjunctionp exp)
           (implicationp (first-operand exp))
           (eq (cadadr exp) (caddr exp)))
      (caddr (cadr exp))
      exp))

(defun modus-tollens (exp)
  "Modus Tollens inference rule ::
   (^ (=> p q) (~ p)) => (~ q)"
  (if (and (conjunctionp exp)
           (implicationp (first-operand exp))
           (or (equalp (list '~ (cadadr exp))
                       (caddr exp))
               (equalp (cadadr exp)
                       (list '~ (caddr exp)))))
      (list '~ (caddr (cadr exp)))
      exp))

(defun syllogism-disjunctive (exp)
  "Syllogism Disjunctive inference rule ::
   (^ (v p q) (~ p)) => q"
  (if (and (conjunctionp exp)
           (disjunctionp (cadr exp))
           (negationp (caddr exp))
           (find-if #'(lambda (x)
                        (equalp x (first-operand (second-operand exp))))
                    (operands (first-operand exp))))
      (car (remove-if #'(lambda (x)
                          (equalp x (first-operand (second-operand exp))))
                      (operands (first-operand exp))))
      exp))

(defun addiction (exp p)
  "Addiction in inference rule ::
   p => (v p q)"
  (list 'v exp p))

(defun conjunction (exp p)
  "Conjunction inference rule ::
   p => (^ p q)"
  (list '^ exp p))

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
           (implicationp (cadr exp))
           (implicationp (caddr exp))
           (or (equalp (second-operand (first-operand exp))
                       (first-operand (second-operand exp)))
               (equalp (first-operand (first-operand exp))
                       (second-operand (second-operand exp)))))
      (if (equalp (second-operand (first-operand exp))
                  (first-operand (second-operand exp)))
          (list '=>
                (first-operand (first-operand exp))
                (second-operand (second-operand exp)))
          (list '=>
                (first-operand (second-operand exp))
                (second-operand (first-operand exp))))
      exp))

(defun absorption (exp)
  "Absorption inference rule ::
  (=> p q) => (=> p (^ p q))"
  (if (implicationp exp)
      (list '=>
            (first-operand exp)
            (list '^
                  (first-operand exp)
                  (second-operand exp)))))
