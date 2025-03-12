;;; early-init.el --- Early Initialization -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; If started with --debug-init, then also activate `debug-on-error'
(setq debug-on-error (and (not noninteractive)
                          init-file-debug))

;; Calculate startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Loaded in %.03fs"
                     (float-time
                      (time-subtract after-init-time before-init-time)))))

;; Disable GC during startup
(let ((original-gc-cons-threshold gc-cons-threshold)
      (original-gc-cons-percentage gc-cons-percentage)
      (original-file-name-handler-alist file-name-handler-alist))
  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-threshold ,original-gc-cons-threshold
                     gc-cons-percentage ,original-gc-cons-percentage
                     file-name-handler-alist ',original-file-name-handler-alist)))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0
        file-name-handler-alist nil))

;; Disable startup stuff that we don't want
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-default-init t
      package-enable-at-startup nil
      initial-major-mode 'fundamental-mode)

;; Doom says this is faster than calling the corresponding functions. See
;; core-ui.el
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(push '(internal-border-width . 8) default-frame-alist)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;;; early-init.el ends here
