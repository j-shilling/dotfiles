;; Make sure quicklisp is loaded
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(pushnew (truename "~/.stumpwm.d/") ql:*local-project-directories*)
(ql:register-local-projects)

(run-shell-command "~/.stumpwm.d/setup-screens.olympus.sh")
(ql:quickload "stumpwm-init")
