(load "fix-quicklisp")
(ql:quickload :prove)
(ql:quickload :lisp-inference)
(in-package :lisp-inference)
(sb-ext:save-lisp-and-die "lisp-inference"
                          :toplevel #'main
                          :executable t
                          :compression 9)
