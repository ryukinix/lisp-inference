(load "fix-quicklisp")
(ql:quickload '(:prove :lisp-inference/test) :silent t)
(setf prove:*enable-colors* t)
(if (prove:run "t/test.lisp")
    (sb-ext:exit :code 0)
    (sb-ext:exit :code 1))
