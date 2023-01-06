;;; config-completion.el --- Setup Completion -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(setq completion-cycle-threshold nil)
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(use-package orderless
  :custom
  (completion-styles
   '(orderless basic))
  (completion-category-overrides
   '((project-file (styles (partial-completion basic orderless)))
     (file (styles (partial-completion basic orderless))))))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package vertico-multiform
  :straight nil
  :after vertico
  :hook (after-init . vertico-multiform-mode)
  :init
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          (info-menu buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-lsp-symbols buffer)
          (consult-xref buffer)
          (embark-keybinding buffer)
          (consult-location buffer))
        vertico-multiform-commands
        '((telega-chat-with buffer)
          (magit:--author flat)
          (Info-goto-node buffer)
          (info-lookup-symbol buffer)
          (Info-follow-reference buffer)
          (consult-yank-pop buffer))))

(use-package vertico-directory
  :straight nil
  :after vertico)

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :hook
  ((after-init . all-the-icons-completion-mode)
   (marginalia-mode . all-the-icons-completion-marginalia-setup)))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-preselect nil)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-quit-at-boundary t))

(use-package corfu-popupinfo
  :straight nil
  :hook
  (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-echo
  :straight nil
  :hook
  (corfu-mode . corfu-echo-mode)
  :custom
  (corfu-echo-delay 0))

(use-package corfu-history
  :straight nil
  :hook
  (corfu-mode . corfu-history-mode))

(use-package corfu-terminal
  :hook
  (after-init . (lambda ()
                  (unless (display-graphic-p)
                    (corfu-terminal-mode)))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions 'cape-dabbrev)
  (add-hook 'completion-at-point-functions 'cape-ispell)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'cape-symbol nil t)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'cape-keyword nil t)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'cape-history nil t))))

(use-package pcmpl-args
  :hook
  (eshell-mode . (lambda ()
                   (require 'pcmpl-args))))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-become))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :bind
  (("M-/" . hippie-expand))
  :init
  (setq yas-verbosity 2)
  (setq hippie-expand-try-functions-list
        '(yas/hippie-try-expand
          try-complete-file-name-partially
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers)))
(use-package yasnippet-snippets)
(use-package auto-yasnippet)
(use-package consult-yasnippet
  :bind
  (("C-M-/" . consult-yasnippet)))

(provide 'config-completion)
;;; config-completion.el ends here
