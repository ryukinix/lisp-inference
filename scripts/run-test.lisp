(ql:quickload :lisp-inference/tests :silent t)

(defun rove/utils/reporter::print-source-location-as-file-path (stream file line column)
  (declare (ignore column))
  (format stream "~&at ~A~@[:~A~]~%"
          (rove/utils/reporter::enough-namestring* file)
          line))
(setf rove:*enable-colors* t)
(if (rove:run :lisp-inference/tests)
    (sb-ext:exit :code 0)
    (sb-ext:exit :code 1))
