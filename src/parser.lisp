;; -*- mode: lisp  -*-
;; Manoel Vilela


(in-package :lisp-inference)

;; selectors
(defun first-operand (x)
  (cadr x))

(defun second-operand (x)
  (caddr x))

(defun first-of-first-operand (x)
  (first-operand
   (first-operand x)))

(defun first-of-second-operand (x)
  (first-operand
   (second-operand x)))

(defun second-of-second-operand (x)
  (second-operand
   (second-operand x)))

(defun second-of-first-operand (x)
  (second-operand
   (first-operand x)))

(defun operator (x)
  (car x))

(defun operands (x)
  (cdr x))

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

(defun binary-operationp (exp)
  (and (listp exp)
       (= (length (operands exp)) 2)
       (valid-operatorp (operator exp))))

(defun valid-operationp (exp)
  (or (unary-operationp exp)
      (binary-operationp exp)))

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


(defun infix-to-prefix (exp)
  "INFIX-TO-PREFIX translate a infix expression to a prefix expression.

This function assumes that exp it is not ambiguous.
In that case, use a completly 'parenthesed' expression

Returns a new prefixed list.
"
  (cond ((atom exp) exp)
        ((and (listp exp)
              (= 2 (length exp)))
         (list (car exp)
               (infix-to-prefix (cdr exp))))
        ((null (cdr exp)) (infix-to-prefix (car exp)))
        (t (list (cadr exp)
                 (infix-to-prefix (car exp))
                 (infix-to-prefix (cddr exp))))))
