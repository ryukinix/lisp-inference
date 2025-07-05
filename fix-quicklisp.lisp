(eval-when (:load-toplevel :execute)
  (pushnew (truename (sb-unix:posix-getcwd/)) ql:*local-project-directories* )
  (ql:register-local-projects)

  ;; install ultralisp if necessary
  (unless (member "ultralisp" (ql-dist:all-dists)
                  :key 'ql-dist:name
                  :test 'string=)
    (ql-dist:install-dist "http://dist.ultralisp.org/"
                          :prompt nil)))
