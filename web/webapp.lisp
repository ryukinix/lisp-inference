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
(defvar *notes*
  '("Please, don't be evil. Use less than 10 variables."
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
var prop = document.getElementById('prop-input').value || document.getElementById('prop-input').placeholder;
var url = window.location.origin + window.location.pathname + '?prop=' + encodeURIComponent(prop);
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


(defun truth-table (exp)
  (with-output-to-string (s)
    (let ((inference:*output-stream* s))
      (inference:print-truth-table (inference:parse-logic exp)))))

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
  (reblocks/html:with-html ()
    (:h1 :align "center" "Lisp Inference Truth Table System")
    (:div :align "center"
          (reblocks-ui/form:with-html-form (:POST (lambda (&key prop &allow-other-keys)
                                   (update-table table prop)))
            (:input :type "text"
                    :id "prop-input"
                    :style "text-align:center;"
                    :name "prop"
                    :placeholder (prop table))
            (:input :type "submit"
                    :value "Eval")
            (:input :type "button"
                    :value "Share"
                    :onclick (js-share-button-function)))
          (:input :type "text" :id "share-url" :style "display: none; width: 100%; margin-top: 10px;" :readonly "readonly")
          (:pre :style "font-size: 20px" (truth table))
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
  (reblocks/html:with-html ()
    (:pre string)))

(defmethod reblocks/page:init-page ((app truth-table) (url-path string) expire-at)
  (declare (ignorable app url-path expire-at))
  (let ((prop (get-parameter "prop")))
    (create-table (if (or (null prop) (string= prop ""))
                      *proposition*
                      prop))))

(defun start (&optional (port *port*))
  (reblocks/debug:on)
  (reblocks/server:stop)
  (reblocks/server:start :port port))

(defun stop ()
  (reblocks/server:stop))
