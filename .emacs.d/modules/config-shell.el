;;; config-shell.el -- Eshell configuration -*- lexical-binding: t; -*-

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
                            (setq xterm-color-preserve-properties t)))
  :commands
  (xterm-color-filter)
  :init
  (require 'esh-mode)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  :config
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
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
  :preface
  (defun project-vterm ()
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
      (let (vterm-buffer (get-buffer vterm-buffer-name))
        (if (and vterm-buffer (not current-prefix-arg))
            (pop-to-buffer vterm-buffer
                           (bound-and-true-p display-comint-buffer-action))
          (vterm)))))
  :init
  (add-to-list 'project-switch-commands '(project-vterm "Vterm" "t") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
  :config
  (setq vterm-copy-exclude-prompt t
        vterm-max-scrollback 100000))

(provide 'config-shell)
;;; config-shell.el ends here
