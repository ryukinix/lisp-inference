;; -*- mode: lisp  -*-
;; Manoel Vilela

(in-package :lisp-inference)

(defparameter *truth-string* "T")
(defparameter *false-string* "F")
(defparameter *output-stream* nil
  "Default stream to write the results")

(defun propositionp (symbol)
  "Check if the given SYMBOL can be a proposition (letters)"
  (and (atom symbol)
       (not (valid-operatorp symbol))))

(defun set-of-propositions (exp)
  "Given a propositional expression EXP return the list of
   propositions used in that expression.
   Ex.: (set-of-propositions '(=> (p q) r)) => '(p q r)"
  (labels ((flatten (tree)
             (if (atom tree)
                 (list tree)
                 (loop for a in tree
                       appending (flatten a)))))
    (remove-if-not #'propositionp (remove-duplicates (flatten exp)))))

(defun get-ordered-propositions (exp)
  "Get the set of propositions in lexicographical order of EXP"
  (sort (set-of-propositions exp) #'string<))

(defun list-of-bits (integer)
  "Given a integer i return a list of bits
  Ex.: (integer-to-bits 3) => (1 1)
  (integer-to-bits 5) => (1 0 1)"
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))
    (or bits '(0))))


(defun cases-to-eval (n-propositions)
  "Based a number of propositions get the cases for each proposition
   N-PROPOSITIONS is a number."
  (let ((n-cases (1- (expt 2 n-propositions))))
    (labels ((complete-with-zeros (case)
               (if (>= (length case) (integer-length n-cases))
                   case
                   (complete-with-zeros (cons 0 case))))
             (prepare-case (case) (mapcar #'zerop (complete-with-zeros case))))
      (mapcar #'prepare-case (loop for x from 0 to n-cases collect (list-of-bits x))))))


(defun group-cases-to-propositions (exp)
  (let* ((propositions (get-ordered-propositions exp))
         (n-propositions (length propositions))
         (cases (cases-to-eval n-propositions)))
    (loop for case in cases collect
                            (loop for n from 0 below n-propositions collect
                                                                    (list (nth n propositions) (nth n case))))))


;; helpers functions
(defun only-leafp (sub-tree)
  "Check if the tree only have leaves"
  (not (nested-listp sub-tree)))

(defun has-suboperations (sub-tree)
  "Check if the tree SUB-TREE has operations in it"
  (nested-listp sub-tree))

;; Although the above functions is only used on stack-of-expressions
;; the function is O(2^n) time, so I think is not a good idea put inside of it.

;; RECURSIVE BOMB, BE CAREFUL
(defun stack-of-expressions (exp)
  "Based on propositional EXP generate a stack of expressions
   to be evaluated on truth-table generation"
  (cond ((and (valid-operationp exp)
              (only-leafp exp)) (list exp))
        ((has-suboperations exp)
         (let ((head-search (stack-of-expressions (car exp)))
               (tail-search (stack-of-expressions (cdr exp)))
               (expressions nil))
           (when (valid-operationp exp)
             (push exp expressions))
           (when (not (null head-search))
             (setq expressions (append head-search expressions)))
           (when (not (null tail-search))
             (setq expressions (append tail-search expressions)))
           expressions))
        (t nil)))


(defun eval-operations (exp-tree)
  "Generate all the truth-table cases and evaluated it based on EXP-TREE"
  (let ((cases (group-cases-to-propositions exp-tree)))
    (labels ((eval-expression (exp case)
               (loop for pair in case do
                 (let ((prop (car pair))
                       (value (cadr pair)))
                   (nsubst value prop exp)))
               (eval exp)))
      (let ((exps (stack-of-expressions exp-tree)))
        (loop for case in cases
              collect (append case
                              (loop for exp in exps
                                    collect (list exp
                                                  (eval-expression (copy-tree exp)
                                                                   case)))))))))


(defun pretty-values (v)
  (if (not (null v))
      *truth-string*
      *false-string*))

(defun prepare-table (evaluated-cases)
  "Get the evaluated cases after EVAL-OPERATIONS
   and generate a list with CAR being the proposition
   and CDR the cases evaluated."
  (let ((header (loop for x in (car evaluated-cases)
                      collect (prefix-to-infix (car x)))))
    (loop for n-exp from 0 below (length header)
          collect (cons (nth n-exp header)
                        (loop for case in evaluated-cases
                              collect (pretty-values (cadr (nth n-exp case))))))))

(defun intern-symbol (s)
  "Get a internal symbol reference of S"
  (intern (symbol-name s) :lisp-inference))

(defun lookup-internal-operators (exp)
  "Ensure that all operators it's inside of :lisp-inference"
  (loop for op in *valid-operators*
        for op-name = (intern (symbol-name op))
        do (nsubst (intern-symbol op-name) op-name exp)
        finally (return exp)))

(defun princ-n (string &optional (n 1))
  "Just print the STRING by N times"
  (dotimes (_ n)
    (format *output-stream* "~a" string)))

(defun print-bar (spaces)
  (princ-n "+" 1)
  (princ-n "-" (1- (reduce #'+ spaces)))
  (princ-n "+" 1)
  (princ-n #\newline))

(defun last-element (l)
  (car (last l)))

(defun eval-expression (exp)
  "Return the boolean values of EXP
Ex.: (eval-expression (=> p q))
'TFTT'
"
  (let* ((tt (prepare-table (eval-operations (lookup-internal-operators exp))))
         (result (last-element tt)))
    (apply #'concatenate 'string (cdr result))))

(defun equal-expression (exp1 exp2)
  "Check if the two expressions have the same truth tables.
The result is the same as check if (exp1 <=> exp2) results in
a tautology."
  (equal (eval-expression exp1)
         (eval-expression exp2)))

(defun print-truth-table (exp)
  "Given a EXP with prefixed notation generate
   a pretty truth-table for each grouped case."
  (let* ((evaluated-cases (eval-operations (lookup-internal-operators exp)))
         (truth-table (prepare-table evaluated-cases))
         (header (loop for column in truth-table collect (car column)))
         (n-values (length (cadr truth-table)))
         (printable-header (loop for x in header
                                 for p = (princ-to-string x)
                                 collect (concatenate 'string "  " p "  |")))
         (spaces (mapcar #'length printable-header)))
    (print-bar spaces)
    (princ-n "|")
    (loop for exp in printable-header
          do (princ-n exp)
          finally (princ-n #\newline))
    (print-bar spaces)
    (loop for n-value from 1 below n-values
          do (progn
               (princ-n "|")
               (loop for n-exp from 0 below (length header)
                     do (let* ((space (nth n-exp spaces))
                               (half-space (floor (- space 2) 2))
                               (val (nth n-value (nth n-exp truth-table))))
                          (princ-n " " half-space)
                          (princ-n val)
                          (princ-n " " half-space)
                          (if (oddp space)
                              (princ-n " |")
                              (princ-n "|"))))
               (princ-n #\newline)))
    (print-bar spaces)))

(defmacro truth (exp)
  "A easy way to generate a truth table"
  `(print-truth-table (quote ,exp)))

(defmacro truth-infix (exp)
  "A easy and infix way of EXP generate a truth table.
   Ex.: (truth-infix (p ^ q)) "
  `(print-truth-table (infix-to-prefix (quote , exp))))


(defun main ()
  (format t "Example of usage: (p ^ q)~%Operators: ~a ~%" *valid-operators*)
  (let ((*output-stream* *standard-output*))
   (handler-case (loop do (princ-n "TRUTH-TABLE> ")
                       do (force-output *output-stream*)
                       do (print-truth-table (infix-to-prefix (read))))
     (end-of-file () )
     #+sbcl (sb-sys:interactive-interrupt () nil))

   (format t "~%Goodbye!~%")))
