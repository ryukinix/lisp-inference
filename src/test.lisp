;; -*- mode: lisp  -*-
;; Manoel Vilela

(in-package :lisp-inference)

(defparameter *equivalence-tests* '((de-morgan (^ p q))
                                    (de-morgan (~ (v p q)))
                                    (de-morgan (~ (^ (~ p) (~ q))))
                                    (double-negation (~ (~ p)))
                                    (double-negation p)))

(defparameter *inference-tests* '((modus-ponens (^ (=> p q) p))
                                  (modus-tollens (^ (=> p q) (~ p)))
                                  (syllogism-disjunctive (^ (v p q) (~ p)))
                                  (syllogism-hypothetical (^ (=> x y) (=> y z)))
                                  (addiction p q)
                                  (conjunction (=> p q) p)
                                  (absorption (=> r (^ x y)))
                                  (simplification-first (^ p q))
                                  (simplification-second (^ r s))))

(defun pretty-test-eval (test symbol)
  (let* ((function (car test))
         (args (cdr test))
         (before (cdr test))
         (result (apply function args))
         (after (prefix-to-infix result)))
    (when (null (cdr before))
      (setq before (prefix-to-infix (car before))))
    (format t "~a :: ~a ~a ~a ~c" function before symbol after #\newline)))

(defun tests-run (tests symbol)
  (mapcar #'(lambda (x) (pretty-test-eval x symbol))
          tests))

(defun tests ()
  (format t "== EQUIVALENCE TESTS == ~c ~c" #\newline #\newline)
  (tests-run *equivalence-tests* '<=>)
  (format t "~c== INFERENCE TESTS == ~c ~c" #\newline #\newline #\newline)
  (tests-run *inference-tests* '=>))
