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

(defparameter *valid-operators* '(~ ^ <=> => v))

;; operation checkers
(defun operationp (exp op)
  "Based a 'op that can be a symbol, verify if the list
   can be a operation of 'op"
  (eq (operator exp) op))

(defun valid-operatorp (op)
  (if (find op *valid-operators*)
      t
      nil))

(defun unary-operationp (exp)
  (and (listp exp)
       (= (length (operands exp)) 1)
       (valid-operatorp (operator exp))))

(defun valid-operationp (exp)
  (or (unary-operationp exp)
      (binary-operationp exp)))

(defun binary-operationp (exp)
  (and (listp exp)
       (= (length (operands exp)) 2)
       (valid-operatorp (operator exp))))

(defun unary-operationp (exp)
  (and (listp exp)
       (= (length (operands exp)) 1)
       (valid-operatorp (operator exp))))

(defun binary-operationp (exp)
  (and (listp exp)
       (= (length (operands exp)) 2)
       (valid-operatorp (operator exp))))

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

(defun swap-operand-operator (exp)
  (if (and (listp exp)
           (binary-operationp exp))
      (list (first-operand exp) (operator exp) (second-operand exp))
      exp))

(defun nested-listp (exp)
  (and (not (atom exp))
       (some #'listp exp)))

(defun prefix-to-infix (exp)
  (cond ((atom exp) exp)
        ((and (nested-listp exp)
              (not (unary-operationp exp)))
         (let ((op (operator exp))
               (a (first-operand exp))
               (b (second-operand exp)))
            (list (prefix-to-infix a)
                  op
                  (prefix-to-infix b))))
        ((and (nested-listp exp)
              (unary-operationp exp))
         (let ((op (operator exp))
               (a (first-operand exp)))
           (cons op (list (prefix-to-infix a)))))
        (t (swap-operand-operator exp))))
