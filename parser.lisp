;; Common Lisp Script
;; Manoel Vilela

;; selectors
(setf (symbol-function 'first-operand) #'cadr)
(setf (symbol-function 'second-operand) #'caddr)
(setf (symbol-function 'first-of-first-operand) #'(lambda (x) (first-operand
                                                          (first-operand x))))
(setf (symbol-function 'first-of-second-operand) #'(lambda (x) (first-operand
                                                           (second-operand x))))
(setf (symbol-function 'second-of-second-operand) #'(lambda (x) (second-operand
                                                            (second-operand x))))
(setf (symbol-function 'second-of-first-operand) #'(lambda (x) (second-operand
                                                           (first-operand x))))
(setf (symbol-function 'operator) #'car)
(setf (symbol-function 'operands) #'cdr)


;; operation checkers
(defun operationp (exp op)
  "Based a 'op that can be a symbol, verify if the list
   can be a operation of 'op"
  (eq (operator exp) op))

(defun unary-operationp (exp)
  (and (not (atom exp))
       (= (length (operands exp)) 1)))

(defun binary-operationp (exp)
  (and (not (atom exp))
       (= (length (operands exp)) 2)))

(defun negationp (exp)
  "Verify if the expression is a negation"
  (and (unary-operationp exp)
       (operationp exp '~)))

(defun conjunctionp (exp)
  "Verify if the expression is a conjunction"
  (and (binary-operationp exp)
       (operationp exp '^)))

(defun disjunctionp (exp)
  "Verify if the expression is a disjunction"
  (and (binary-operationp exp)
       (operationp exp 'v)))

(defun implicationp (exp)
  "Verify if the expression is an implication"
  (and (binary-operationp exp)
       (operationp exp '=>)))

(defun biconditionalp (exp)
  "Verify if the expression is a biconditional"
  (and (binary-operationp exp)
       (operationp exp '<=>)))
