(eval-when (:load-toplevel :execute)
  (pushnew (truename (sb-unix:posix-getcwd/)) ql:*local-project-directories* )
  (ql:register-local-projects))
