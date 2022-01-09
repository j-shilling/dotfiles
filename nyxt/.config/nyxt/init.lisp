(in-package #:nyxt-user)

;;; Load quicklisp. Not sure it works.
#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :slynk)
(load-after-system :slynk "~/.config/nyxt/slynk.lisp")

(dolist (file (list (nyxt-init-file "statusline.lisp")
                    (nyxt-init-file "stylesheet.lisp")))
  (load file))

(define-configuration browser
    ((session-restore-prompt :never-restore)))
(define-configuration (buffer internal-buffer editor-buffer )
  ((default-modes `(vi-normal-mode ,@%slot-default%))))
(define-configuration prompt-buffer
  ((default-modes `(vi-insert-mode ,@%slot-default%))))
