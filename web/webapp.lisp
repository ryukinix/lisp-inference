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
           #:*propostion*
           #:*port*)
  (:nicknames #:webapp))

(in-package lisp-inference/web)

(defvar *proposition* '(P => Q) "Default proposition")
(defvar *port* (find-port:find-port))

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
      (inference:print-truth-table (inference:infix-to-prefix exp)))))

(defun create-table (exp)
  (make-instance 'table
                 :prop (format nil "~a" exp)
                 :truth (truth-table exp)))

(defun update-table (table exp)
  (setf (prop table) (format nil "~a" exp))
  (setf (truth table) (truth-table exp)))

(defgeneric update-proposition (table exp))

(defmethod update-proposition (table (exp list))
  (update-table table exp)
  (update table))

(defmethod update-proposition (table (string string))
  (update-proposition
    table
    (mapcar (lambda (x)
              (intern (string-upcase x)))
            (str:words (string-trim '(#\( #\)) string)))))

(defmethod render ((table table))
  (with-html
    (:h1 "Lisp Inference Truth Table System")
    (with-html-form (:POST (lambda (&key prop &allow-other-keys)
                             (update-proposition table prop)))
      (:input :type "text"
              :name "prop"
              :placeholder (prop table))
      (:input :type "submit"
              :value "Eval"))
    (:pre (truth table))))

(defmethod render ((string string))
  (with-html
    (:pre string)))

(defmethod weblocks/session:init ((app truth-table))
  (declare (ignorable app))
  (create-table *proposition*))

(defun start (&optional (port *port*))
  (weblocks/debug:on)
  (weblocks/server:stop)
  (weblocks/server:start :port port))

(defun stop ()
  (weblocks/server:stop))
