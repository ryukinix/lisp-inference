;; -*- mode: lisp  -*-
;; Manoel Vilela

;;;; lisp-inference.asd

(asdf:defsystem #:lisp-inference
  :description "An Inference Engine using Propositional Calculus"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "operators")
               (:file "parser")
               (:file "equivalences"
                :depends-on ("parser" "operators"))
               (:file "inferences"
                :depends-on ("parser" "operators"))
               (:file "truth-table"
                :depends-on ("parser" "operators" "equivalences"))
               (:file "test" :depends-on ("equivalences" "inferences"))))
