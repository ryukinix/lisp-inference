(defpackage #:lisp-inference/tests/test-equivalence-rules
  (:use #:cl
        #:lisp-inference
        #:rove))

(defpackage #:lisp-inference/tests/test-inference-rules
  (:use #:cl
        #:lisp-inference
        #:rove))

(defpackage #:lisp-inference/tests/test-infix-parsing
  (:use #:cl
        #:lisp-inference
        #:rove))

(defpackage #:lisp-inference/tests/test-pratt
  (:use #:cl
        #:lisp-inference
        #:rove))

(defpackage #:lisp-inference/tests
  (:use #:cl)
  (:import-from #:lisp-inference/tests/test-equivalence-rules)
  (:import-from #:lisp-inference/tests/test-inference-rules)
  (:import-from #:lisp-inference/tests/test-infix-parsing)
  (:import-from #:lisp-inference/tests/test-pratt))
