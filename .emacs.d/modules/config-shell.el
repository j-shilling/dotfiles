;;; config-shell.el -- Eshell configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package tramp
  :straight nil
  :config
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package eshell
  :straight nil
  :bind
  ("s-e" . eshell)
  :config
  (setq eshell-directory-name (init-state-path "eshell")
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil)

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (defun config-shell-require-tramp ()
    (require 'tramp)
    (remove-hook 'eshell-mode-hook #'config-shell-require-tramp))
  (add-hook 'eshell-mode-hook #'config-shell-require-tramp)

  ;; History Management
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t)

  (add-to-list 'eshell-modules-list 'eshell-hist)

  ;; Aliases
  (setq eshell-aliases-file (init-state-path "eshell" "alias"))

  ;; Buffer Management
  (require 'esh-mode)
  (setq eshell-buffer-maximum-lines 10240)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Smart Shell
  (add-to-list 'eshell-modules-list 'eshell-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient -c -a ''"))

(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode))

(use-package eshell-syntax-highlighting
  :hook
  (eshell-mode . eshell-syntax-highlighing-mode))

(use-package xterm-color
  :hook
  (eshell-before-prompt . (lambda ()
                            (require 'xterm-color)
                            (defvar xterm-color-preserve-properties)
                            (setq xterm-color-preserve-properties t)))
  :commands
  (xterm-color-filter)
  :init
  (with-eval-after-load 'esh-mode
    (defvar eshell-preoutput-filter-functions)
    (defvar eshell-output-filter-functions)
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))
  (with-eval-after-load 'comint
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output
                  comint-output-filter-functions))
    (add-hook 'comint-mode-hook
              ;; Taken from https://github.com/atomontage/xterm-color#comint
              (lambda ()
                (font-lock-mode -1)
                (make-local-variable 'font-lock-function)
                (setq font-lock-function (lambda (_) nil))
                (add-hook 'comint-preoutput-filter-functions
                          'xterm-color-filter nil t))))
  :config
  (setenv "TERM" "xterm-256color"))

(use-package envrc
  :hook
  (after-init . envrc-global-mode))

(use-package vterm
  :bind
  (([remap term] . vterm)
   ([remap shell] . vterm)
   ([remap project-shell] . project-vterm)
   ("s-t" . vterm)
   ("C-x p t" . project-vterm))
  :custom
  (vterm-copy-exclude-prompt t)
  (vterm-max-scrollback 100000)
  :preface
  (defun project-vterm ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (setq-local vterm-buffer-name (project-prefixed-buffer-name "vterm"))
      (setq-local vterm-buffer-name-string nil)
      (let (vterm-buffer (get-buffer vterm-buffer-name))
        (if (and vterm-buffer (not current-prefix-arg))
            (pop-to-buffer vterm-buffer
                           (bound-and-true-p display-comint-buffer-action))
          (vterm)))))
  :init
  (with-eval-after-load 'project
    (defvar project-switch-commands)
    (defvar project-kill-buffer-conditions)
    (declare-function assq-delete-all (key alist))
    (customize-set-value
     'project-switch-commands
     (cons '(project-vterm "Vterm") project-switch-commands))
    (customize-set-value
     'project-kill-buffer-conditions
     (cons '(major-mode . vterm-mode) project-kill-buffer-conditions))))

(provide 'config-shell)
;;; config-shell.el ends here
