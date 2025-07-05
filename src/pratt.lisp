(in-package :lisp-inference)

(defparameter *binding-precedence*
  '(
    ("^" . 80) 
    ("v" . 70)
    ("[+]" . 60) 
    ("=>" . 50)
    ("<=>" . 40)))

(defvar *tokens* nil)
(defvar *pos* 0)


(defun read-token (stream)
  (cond
    ((peek-char nil stream nil nil)
     (let ((c (peek-char nil stream)))
       (cond
         ((find c "()") (list (string (read-char stream))))
         ((char= c #\~) (list (string (read-char stream))))
         ((member c '(#\Space #\Tab #\Newline)) (read-char stream) (read-token stream))
         ((alpha-char-p c)
          (let ((sym (read stream)))
                      (list (string-downcase (string sym)))))
         (t
          (let ((token (with-output-to-string (out)
                         (loop for ch = (peek-char nil stream nil nil)
                               while (and ch (find ch "<=>[+]"))
                               do (write-char (read-char stream) out)))))
            (if (string= token "") (list (string (read-char stream))) (list token)))))))))

(defun tokenize (input)
  (with-input-from-string (in input)
    (loop for token = (read-token in)
          while token
          append token)))

(defun next-token () 
  (nth *pos* *tokens*))

(defun advance () 
  (prog1 (next-token) (incf *pos*)))

(defun match (tok)
  (when (equal (next-token) tok) (advance) t))

(defun get-binding (tok)
  (or (cdr (assoc tok *binding-precedence* :test #'string=)) 0))

(defun nud (token)
  (cond
    ((string= token "~") `(~ ,(parse-expression 90)))
    ((string= token "(")
     (prog1 (parse-expression) (match ")")))
    (t (intern (string-upcase token)))))

(defun led (token left)
  (let ((right (parse-expression
                (if (member token '("=>" "<=>") :test #'string=)
                    (1- (get-binding token)) ;; Right-associative
                    (get-binding token)))))
    (list (intern (string-upcase token)) left right)))

(defun parse-expression (&optional (rbp 0))
  (let* ((token (advance))
         (left (nud token)))
    (loop while (and (next-token) (< rbp (get-binding (next-token))))
          do (setf left (led (advance) left)))
    left))

;; entrypoint
(defun parse-logic (input)
  (setf *tokens* (tokenize input)
        *pos* 0)
  (parse-expression))