;; Common Lisp Script
;; Manoel Vilela

;; selectors
(setf (symbol-function 'first-operand) #'cadr)
(setf (symbol-function 'second-operand) #'caddr)
(setf (symbol-function 'operator) #'car)
(setf (symbol-function 'operands) #'cdr)


;; operation checkers
(defun operationp (exp op)
  "Based a 'op that can be a symbol, verify if the list
   can be a operation of 'op"
  (and (not (atom exp))
       (eq (operator exp) op)
       (not (null (first-operand exp)))))

(defun negationp (exp)
  "Verify if the expression is a negation"
  (operationp exp '~))

(defun conjunctionp (exp)
  "Verify if the expression is a conjunction"
  (operationp exp '^))

(defun disjunctionp (exp)
  "Verify if the expression is a disjunction"
  (operationp exp 'v))

(defun implicationp (exp)
  "Verify if the expression is an implication"
  (operationp exp '=>))

(defun biconditionalp (exp)
  "Verify if the expression is a biconditional"
  (operationp exp '<=>))
