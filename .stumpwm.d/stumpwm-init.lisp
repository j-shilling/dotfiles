(in-package :stumpwm-init)

;; Basics
(setf *startup-message* "Ready to go"
      *shell-program* (getenv "SHELL")
      *mouse-focus-policy* :click)

(when *initializing*
  (which-key-mode))
