;; -*- mode: lisp  -*-
;; Manoel Vilela

;;;; package.lisp

(defpackage #:lisp-inference
  (:use #:cl #:cl-user)
  (:export #:double-negation ;; equivalences
           #:de-morgan
           #:modus-ponens    ;; inferences
           #:modus-tollens
           #:syllogism-disjunctive
           #:addiction
           #:conjunction
           #:simplification-first
           #:simplification-second
           #:syllogism-hypothetical
           #:absorption     ;; parser
           #:propositionp
           #:operationp
           #:unary-operationp
           #:binary-operationp
           #:valid-operationp
           #:binary-operationp
           #:negationp
           #:conjunctionp
           #:disjunctionp
           #:implicationp
           #:biconditionalp
           #:~            ;; operators
           #:^
           #:v
           #:=>
           #:<=>
           #:[+]
           #:make-conjunction
           #:make-negation
           #:make-disjunction
           #:make-implication
           #:make-biconditional
           #:*valid-operators*
           #:prefix-to-infix
           #:infix-to-prefix
           #:print-truth-table ;; truth-table.lisp
           #:eval-expression
           #:equal-expression
           #:truth
           #:truth-infix
           #:*output-stream*
           #:*valid-operators*
           #:main)
  (:nicknames "INFERENCE"))
