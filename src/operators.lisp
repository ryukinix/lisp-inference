;; -*- mode: lisp-mode  -*-
;; Manoel Vilela


(in-package :lisp-inference)

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
  (v (~ p) q))

(defun <=> (p q)
  "Biconditional binary operator"
  (^ (=> p q) (=> q p)))



;; symbolic construction
(defun make-conjunction (p q)
  (list '^ p q))

(defun make-negation (p)
  (list '~ p))

(defun make-disjunction (p q)
  (list 'v p q))

(defun make-implication (p q)
  (list '=> p q))

(defun make-biconditional (p q)
  (list '<=> p q))
