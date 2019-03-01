;; -*- mode: lisp  -*-
;; Manoel Vilela

;;;; lisp-inference.asd


(asdf:defsystem #:lisp-inference
  :description "An Inference Engine using Propositional Calculus"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.2.0"
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
                :depends-on ("parser" "operators" "equivalences"))))

(asdf:defsystem #:lisp-inference/test
  :description "Lisp Inference Test Suit"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.2.0"
  :serial t
  :pathname "t"
  :depends-on (:lisp-inference :prove)
  :components ((:file "test"))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) c)))
