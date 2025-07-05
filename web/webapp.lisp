(defpackage lisp-inference/web
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp)
  (:export #:start
           #:stop
           #:*notes*
           #:*proposition*
           #:*port*)
  (:nicknames #:webapp))

(in-package lisp-inference/web)

(defvar *proposition* "P => Q" "Default proposition")
(defvar *port* (find-port:find-port))
(defvar *notes*
  '("My lexer doesn't work very well with parenthesis."
    "Please, don't be evil. Use less than 10 variables."
    "Yes, [+] it's a XOR. Mathematically: p ⊕ q."
    "(=> ->) and (<=> <->) are aliases."))

(defapp truth-table
  :prefix "/"
  :description "Lisp Inference Truth Table")

(defwidget table ()
  ((prop
    :initarg :prop
    :accessor prop)
   (truth
    :initarg :truth
    :initform nil
    :accessor truth)))

(defun truth-table (exp)
  (with-output-to-string (s)
    (let ((inference:*output-stream* s))
      (inference:print-truth-table exp))))

(defun create-table (exp)
  (make-instance 'table
                 :prop (format nil "~a" exp)
                 :truth (truth-table exp)))

(defgeneric update-table (table exp))

(defmethod update-table (table (exp list))
  (setf (prop table) (format nil "~a" exp))
  (setf (truth table) (truth-table exp)
  (update table))

(defmethod update-table (table (exp string))
  (update-table
   table
   (inference:parse-logic exp)))

(defmethod render ((table table))
  (with-html
    (:h1 :align "center" "Lisp Inference Truth Table System")
    (:div :align "center"
          (with-html-form (:POST (lambda (&key prop &allow-other-keys)
                                   (update-table table prop)))
            (:input :type "text"
                    :name "prop"
                    :placeholder (prop table))
            (:input :type "submit"
                    :value "Eval"))
          (:pre :style "font-size: 25px" (truth table))
          (:pre (format nil "Operators: ~a" inference:*valid-operators*)))
    (:p "Some notes: "
        (:ul
         (loop for note in *notes*
               do (:li (render-note note)))))
    (:span "Source: "
           (:a :href "https://github.com/ryukinix/lisp-inference"
               "ryukinix/lisp-inference"))
    (:br)
    (:span "Documentation: "
           (:a :href
               "https://lerax.me/lisp-inference" "lerax.me/lisp-inference"))))

(defun render-note (string)
  (with-html
    (:pre string)))

(defmethod weblocks/session:init ((app truth-table))
  (declare (ignorable app))
  (create-table (parse-string *proposition*)))

(defun start (&optional (port *port*))
  (weblocks/debug:on)
  (weblocks/server:stop)
  (weblocks/server:start :port port))

(defun stop ()
  (weblocks/server:stop))
