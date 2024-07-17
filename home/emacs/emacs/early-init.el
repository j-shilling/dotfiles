;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1025 10))))

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

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
