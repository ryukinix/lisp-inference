;; auto generate docs with staple
(ql:quickload '(:staple :lisp-inference :lisp-inference/tests :lisp-inference/web) :silent t)

(staple:generate :lisp-inference :if-exists :overwrite)
(format t "Docs generated at docs/ directory.~%")
(sb-ext:exit :code 0)
