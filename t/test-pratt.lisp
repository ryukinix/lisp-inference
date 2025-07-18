(defpackage #:lisp-inference/tests/test-pratt
  (:use #:cl
        #:lisp-inference
        #:rove))


(in-package #:lisp-inference/tests/test-pratt)

(deftest test-pratt
  (testing "== Pratt Parser"
    (ok (equal (parse-logic "p v q")
               '(v p q)))

    (ok (equal (parse-logic "(p v q)")
               '(v p q)))

    (ok (equal (parse-logic "p => q ^ r")
               '(=> p (^ q r))))

    (ok (equal (parse-logic "(p => q) ^ r")
               '(^ (=> p q) r)))

    (ok (equal (parse-logic "p -> ~ q")
               '(-> p (~ q))))

    (ok (equal (parse-logic "p [+] (r => s)")
               '([+] p (=> r s))))

    (ok (equal (parse-logic "~p")
               '(~ p)))
    ))
