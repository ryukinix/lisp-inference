;; -*- mode: lisp  -*-
;; Manoel Vilela


;;;; lisp-inference.asd

(defpackage :lisp-inference/system
  (:use :cl :uiop :asdf))

(in-package :lisp-inference/system)

(defun get-all-test-files ()
  (mapcar #'(lambda (p) (list :file (pathname-name p)))
          (directory-files (system-relative-pathname :lisp-inference "t/")
                           "*test*.lisp")))


(defsystem #:lisp-inference
  :description "An Inference Engine using Propositional Calculus"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.4.0"
  :homepage "https://github.com/ryukinix/lisp-inference"
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

(defsystem #:lisp-inference/web
  :description "An web interface for Lisp Inference Truth Table"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.4.0"
  :homepage "https://github.com/ryukinix/lisp-inference"
  :serial t
  :depends-on (:lisp-inference
               :40ants-logging
               :40ants-routes ;; implicit dependency of reblocks
               :reblocks
               :reblocks-ui
               :clack-handler-hunchentoot
               :find-port
               :str)
  :pathname "web"
  :components ((:file "webapp")))

(defsystem #:lisp-inference/tests
  :description "Lisp Inference Tests"
  :author "Manoel Vilela <manoel_vilela@engineer.com>"
  :license "BSD"
  :version "0.4.0"
  :homepage "https://github.com/ryukinix/lisp-inference"
  :serial t
  :depends-on (:lisp-inference :rove)
  :pathname "t"
  :components #.(get-all-test-files)
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))
