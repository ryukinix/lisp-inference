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
               (:file "pratt")
               (:file "equivalences"
                :depends-on ("parser" "operators"))
               (:file "inferences"
                :depends-on ("parser" "operators"))
               (:file "truth-table"
                :depends-on ("pratt" "parser" "operators" "equivalences"))))

(asdf:defsystem #:lisp-inference/web
  :description "An web interface for Lisp Inference Truth Table"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.2.0"
  :serial t
  :depends-on (:lisp-inference
               :40ants-routes ;; implicit dependency of reblocks
               :reblocks
               :reblocks-ui
               :clack-handler-hunchentoot
               :find-port
               :str)
  :pathname "web"
  :components ((:file "webapp")))

(asdf:defsystem #:lisp-inference/test
  :description "Lisp Inference Test Suit"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.2.0"
  :serial t
  :pathname "t"
  :depends-on (:lisp-inference :rove)
  :components ((:file "test")))
