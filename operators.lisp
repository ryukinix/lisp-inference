;; Common Lisp Script
;; Manoel Vilela

;; operators
(defun ~ (p)
  "Not unary operator"
  (not p))

(defun ^ (p q)
  "Conjuction binary operator"
  (and p q))

(defun v (p q)
  "Disjunction binary operator"
  (or p q))

(defun => (p q)
  "Implication binary operator"
  (v p (~ q)))

(defun <=> (p q)
  "Biconditional binary operator"
  (^ (=> p q) (=> q p)))



