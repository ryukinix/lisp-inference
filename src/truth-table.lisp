;; -*- mode: lisp-mode  -*-
;; Manoel Vilela

(in-package :lisp-inference)

(defun propositionp (symbol)
  "Check if the given symbol can be a proposition (letters)"
  (and (atom symbol)
       (not (valid-operatorp symbol))))

(defun set-of-propositions (exp)
  "Given a propositional expression return the list of
   propositions used in that expression.
   Ex.: (set-of-propositions '(=> (p q) r)) => '(p q r)"
  (labels ((flatten (tree)
             (if (atom tree)
                 (list tree)
                 (loop for a in tree
                       appending (flatten a)))))
          (remove-if-not #'propositionp (remove-duplicates (flatten exp)))))

(defun get-ordered-propositions (exp)
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
  "Based a number of propositions get the cases for each proposition"
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
  (not (nested-listp sub-tree)))

(defun has-suboperations (sub-tree)
  (nested-listp sub-tree))

;; Although the above functions is only used on stack-of-expressions
;; the function is O(2^n) time, so I think is not a good idea put inside of it.

;; RECURSIVE BOMB, BE CAREFUL
(defun stack-of-expressions (exp)
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
  (let ((cases (group-cases-to-propositions exp-tree)))
    (labels ((eval-expression (exp case)
               (loop for pair in case do
                 (let ((prop (car pair))
                        (value (cadr pair)))
                    (nsubst value prop exp)))
               (eval exp)))
      (let ((exps (stack-of-expressions exp-tree)))
        (loop for case in cases
              collect (append case (loop for exp in exps
                                         collect (list exp (eval-expression (copy-tree exp) case)))))))))


(defun pretty-values (v)
  (if (not (null v))
      "T"
      "F"))

(defun prepare-table (evaluated-cases)
  (let ((header (loop for x in (car evaluated-cases) collect (prefix-to-infix (car x)))))
    (loop for n-exp from 0 below (length header)
          collect (cons (nth n-exp header) (loop for case in evaluated-cases
                                                 collect (pretty-values (cadr (nth n-exp case))))))))
(defun princ-n (string n)
  (dotimes (_ n) (princ string)))

(defun print-truth-table (exp)
  (let* ((evaluated-cases (eval-operations exp))
         (truth-table (prepare-table evaluated-cases))
         (header (loop for column in truth-table collect (car column)))
         (n-values (length (cadr truth-table)))
         (printable-header (loop for x in header collect (concatenate 'string "  " (princ-to-string x) "  |")))
         (spaces (mapcar #'length printable-header)))
     ;; (print spaces)
     (loop for exp in printable-header do (princ exp))
     (princ #\newline)
     (princ-n "-" (reduce #'+ spaces))
     (princ #\newline)
     (loop for n-value from 1 below n-values
           do (progn (loop for n-exp from 0 below (length header)
                           do (let* ((space (nth n-exp spaces))
                                     (half-space (floor (- space 2) 2))
                                     (val (nth n-value (nth n-exp truth-table))))
                                (princ-n " " half-space)
                                (princ val)
                                (princ-n " " half-space)
                                (if (oddp space)
                                    (princ " |")
                                    (princ "|"))))
                     (princ #\newline)))
    (princ #\newline)))


(defun main ()
  (print-truth-table '(=> (v p (~ q)) (=> p q)))
  (print-truth-table '(^ p q))
  (print-truth-table '(v p q))
  (print-truth-table '(=> p q))
  (print-truth-table '(<=> p q))
  (print-truth-table `(<=> (^ p q) ,(de-morgan '(^ p q)))))

#| set of manual tests (only for debug)
(eval-operations '(~ p))
(prepare-table (eval-operations '(^ p (v r s))))
(cases-to-eval 3)

(stack-of-expressions '(=> (^ p q) (v s (^ s q)))) ;; '((=> (^ p q) (v s r))
                                                   ;;   (^ p q)
                                                   ;;   (v r s))

(group-cases-to-propositions '(^ (=> p q) r))

(set-of-propositions '(^ (=> p q) r))
|#
