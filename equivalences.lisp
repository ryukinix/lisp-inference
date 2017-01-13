;; Common Lisp Script
;; Manoel Vilela

;; set of equivalences transform for propositional calculus

(load "parser.lisp")
(load "operators.lisp")

(defun double-negation (exp)
  "Double negation equivalence rule ::
   (~ (~ p)) <=> p"
  (if (and (negationp exp)
           (negationp (first-operand exp)))
      (first-of-first-operand exp)
      exp))

(defun clean-double-negations (exp)
  "Search in the expression's tree for double-negations and apply
   the equivalence of double-negation to simplify it."
  (if (nested-listp exp)
      (let ((head (car exp))
            (tail (cdr exp)))
        (when (listp head)
          (setq head (mapcar #'double-negation head)))
        (cons head
              (mapcar #'double-negation (clean-double-negations tail))))
      exp))

(defun de-morgan (exp)
  "De Morgan equivalence rule ::
  (~ (^ p q)) <=> (v (~ p) (~ q))
  (^ p q) <=> (~ (v (~ p) (~ q)))
  (~ (v p q)) <=> (^ (~ p) (~ q))
  (v p q) <=> (~ (^ (~ p) (~ q)))"
  (clean-double-negations
   (cond ((and (negationp exp) ;; (~ (^ p q)) <=> (v (~ p) (~ q))
               (conjunctionp (first-operand exp)))
          (make-disjunction
           (make-negation (first-of-first-operand exp))
           (make-negation (second-of-first-operand exp))))
         ((and (negationp exp) ;; (~ (v p q)) <=> (^ (~ p) (~ q))
               (disjunctionp (first-operand exp)))
          (make-conjunction
           (make-negation (first-of-first-operand exp))
           (make-negation (second-of-first-operand exp))))
         ((conjunctionp exp) ;; (^ p q) <=> (~ (v (~ p) (~ q)))
          (make-negation (make-disjunction (make-negation (first-operand exp))
                                           (make-negation (second-operand exp)))))
         ((disjunctionp exp) ;; (v p q) <=> (~ (^ (~ p) (~ q)))
          (make-negation (make-conjunction (make-negation (first-operand exp))
                                           (make-negation (second-operand exp)))))
         (t exp))))
