(defpackage lisp-inference/web
  (:use #:cl
        #:reblocks-ui/form
        #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks-ui/core
                #:ui-widget)
  (:import-from #:reblocks/page
                #:init-page)
  (:import-from #:reblocks/request
                #:get-parameter)
  (:export #:start
           #:stop
           #:*notes*
           #:*proposition*
           #:*port*)
  (:nicknames #:webapp))
;; reblocks docs: https://40ants.com/reblocks/

(in-package lisp-inference/web)

(defvar *proposition* "P => Q" "Default proposition")
(defvar *port* (find-port:find-port))
(defvar *tautologies*
  '(("de morgan" . "~p v ~q <=> ~(p ^ q)")
    ("modus ponens" . "p ^ (p -> q) => q")
    ("constructive dilemma" . "((p -> q) ^ (r -> s) ^ (p v r)) => (q v s)")
    ("hypothetical syllogism" . "((p -> q) ^ (q -> r)) => (p -> r)")
    ("disjunctive syllogism" . "((p v q) ^ ~p) => q")
    ("implication" . "p -> q <=> ~ p v q")))
(defvar *notes*
  '("Please, don't be evil. Use less than 10 variables."
    "Use spaces around operators. Use 'p ^ q' instead 'p^q'."
    "Yes, [+] it's a XOR. Mathematically: p ⊕ q."
    "(=> ->) and (<=> <->) are aliases."))

(defapp truth-table
  :prefix "/"
  :description "Lisp Inference Truth Table")

(defwidget table (ui-widget)
  ((prop
    :initarg :prop
    :accessor prop)
   (truth
    :initarg :truth
    :initform nil
    :accessor truth)))

(defmacro js-share-button-function ()
  "

// encode parentheses as well to increase link-parsing compatibility in x.com and other platforms
function strictEncodeURIComponent(str) {
  return encodeURIComponent(str)
    .replaceAll('(', '%28')
    .replaceAll(')', '%29');
}

var prop = document.getElementById('prop-input').value
var url = window.location.origin + window.location.pathname + '?prop=' + strictEncodeURIComponent(prop);
var shareUrlInput = document.getElementById('share-url');
shareUrlInput.value = url;
shareUrlInput.style.display = 'block';
try {
  navigator.clipboard.writeText(url).then(function() {
    /* clipboard successfully set */
  }, function() {
    /* clipboard write failed */
  });
} catch (e) {
  // ignore
}
")

(defmacro js-eval-update-query-path ()
  "
const url = location.href.replace(location.search, '')
// history.assign(url);
history.pushState(null, '', url);
"
  )

(defun truth-table (exp)
  (with-output-to-string (s)
    (let ((inference:*output-stream* s))
      (log:info "expression: ~a" exp)
      (handler-case (inference:print-truth-table
                     (inference:parse-logic exp))
        (simple-error (c)
          (apply #'format s
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c)))
        (error (c)
          (declare (ignore c))
          (format s "error: invalid logic expression!"))))))

(defun create-table (exp-string)
  (make-instance 'table
                 :prop exp-string
                 :truth (truth-table exp-string)))

(defgeneric update-table (table exp))

(defmethod update-table (table (exp string))
  (setf (prop table) exp)
  (setf (truth table) (truth-table exp))
  (update table))

(defmethod render ((table table))
  (let ((url-prop (get-parameter "prop")))
    (when (and url-prop (string/= url-prop "") (string/= url-prop (prop table)))
      (update-table table url-prop)))
  (with-html ()
    (html-title)
    (html-truth-table table)
    (html-notes)
    (html-tautologies)
    (html-documentation)))

(defun html-title ()
  (with-html ()
    (:h1 :align "center" "Lisp Inference Truth Table System")))

(defun html-truth-table (table)
  (reblocks/html:with-html ()
    (:div :align "center"
     (with-html-form (:POST (lambda (&key prop &allow-other-keys)
                              (update-table table prop))
                       :extra-submit-code (js-eval-update-query-path))
       (:input :type "text"
        :id "prop-input"
        :style "text-align:center;"
        :name "prop"
        :value (prop table))
       (:input :type "submit"
        :value "Eval")
       (:input :type "button"
        :value "Share"
        :onclick (js-share-button-function)))
     (:input :type "text"
      :id "share-url"
      :style "cursor: text; display: none; width: 100%; margin-top: 10px;"
      :readonly "readonly")
     (:pre :style "font-size: 20px" (truth table))
     (:pre (format nil "Operators: ~a" inference:*valid-operators*)))
    (:br)))

(defun html-documentation ()
  (with-html ()
    (:span "Source: "
     (:a :href "https://github.com/ryukinix/lisp-inference"
      "ryukinix/lisp-inference"))
    (:br)
    (:span "Documentation: "
     (:a :href
      "https://manoel.dev/lisp-inference" "manoel.dev/lisp-inference"))))

(defun html-tautologies ()
  (with-html ()
    (:p "Tautologies: "
     (:ul
      (loop for (tautology . exp) in *tautologies*
            for text := (format nil "~a: ~a" tautology exp)
            collect (:li
                (:a :href
                 (format nil "/?prop=~a" (quri:url-encode exp))
                 text)))))))


(defun html-notes ()
  (with-html ()
    (:p "Some notes: "
     (:ul
      (loop for note in *notes*
            collect (:li (:pre note)))))))

(defun html-render-note (string)
  (with-html ()
    (:pre string)))

(defmethod reblocks/page:init-page ((app truth-table) (url-path string) expire-at)
  (declare (ignorable app url-path expire-at))
  (let ((prop (get-parameter "prop")))
    (create-table (if (or (null prop) (string= prop ""))
                      *proposition*
                      prop))))

(defun start (&key (port *port*) (debug t))
  (if debug
      (reblocks/debug:on)
      (40ants-logging:setup-for-cli :level :info))
  (reblocks/server:stop)
  (reblocks/server:start :port port))

(defun stop ()
  (reblocks/server:stop))
