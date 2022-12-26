;;; init.el --- Configure Emacs -*- lexical-bindings: t -*-
;;
;;; Commentary:
;;
;;; Code:

;;;
;;; System Information
;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;;;
;;; XDG Directories
;;;

(require 'xdg)
(defun init-data-home ()
  "Find the directory for user specific data files."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "APPDATA")
     (xdg-data-home))))

(defun init-state-home ()
  "Find the directory for user specific data files.

This directory is for files less portable or less important than
the ones in `init-data-home'."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg--dir-home "XDG_STATE_HOME" "~/.local/state"))))

(defun init-cache-home ()
  "Find the directory for user specific cache files."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg-cache-home))))

(require 'cl-lib)
(defun init-path (&rest segments)
  "Assemble SEGMENTS into a complete path string."
  (cl-reduce (lambda (acc segment)
               (expand-file-name segment acc))
             segments))

(defun init-data-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-data-home'."
  (apply #'init-path (init-data-home) segments))

(defun init-state-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-state-home'."
  (apply #'init-path (init-state-home) segments))

(defun init-cache-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-cache-home'."
  (apply #'init-path (init-cache-home) segments))

;;;
;;; Package Management
;;;

;; Setup Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(require 'use-package)

;;;
;;; General Setup
;;;

(setq native-comp-deferred-compilation nil)

(setq user-full-name "Jake Shilling"
      user-email-address "shilling.jake@gmail.com")

(setq minibuffer-message-timeout 0)

(setq custom-file (init-cache-path "custom.el"))
(load custom-file t)

(setq backup-directory-alist
      `(,(cons "." (init-cache-path "backup"))))

(setq recentf-save-file (init-cache-path "recentf"))
(recentf-mode 1)
(run-with-idle-timer 30 t 'recentf-save-list)

(setq savehist-file (init-cache-path "history"))
(savehist-mode 1)
(run-with-idle-timer 30 t 'savehist-save)

(setq bookmark-default-file (init-cache-path "bookmarks"))

(column-number-mode 1)
(save-place-mode 1)
(subword-mode 1)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)
(setq require-final-newline t)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(use-package ws-butler
  :hook
  ((text-mode . ws-butler-mode)
   (prog-mode-hook . ws-butler-mode)))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t
        aw-ignore-current nil))

;;;
;;; Appearance
;;;

(set-default 'cursor-type '(bar . 1))
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq bookmark-set-fringe-mark nil)

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
(setq mode-line-format nil)

(set-frame-parameter (selected-frame) 'internal-border-width 8)
(setq use-dialog-box nil
      use-file-dialog nil)

(setq window-divider-default-right-width 8)
(window-divider-mode)

(setq inhibit-startup-screen t)

(use-package menu-bar
  :straight nil
  :config
  (menu-bar-mode 0))

(use-package tool-bar
  :straight nil
  :config
  (tool-bar-mode 0))

(use-package scroll-bar
  :straight nil
  :config
  (scroll-bar-mode 0))

(use-package modus-themes
  :demand t
  :init
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t
        modus-themes-fringes nil
        modus-themes-operandi-color-overrides '((fg-window-divider-inner . "#ffffff")
                                                (fg-window-divider-outer . "#ffffff"))
        modus-themes-vivendi-color-overrides '((fg-window-divider-inner . "#000000")
                                               (fg-window-divider-outer . "#000000")))
  (modus-themes-load-vivendi))

;;;
;;; Completion
;;;

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
   ((project-file (styles (partial-completion basic orderless)))
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
  :config
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 0))

(use-package corfu-popupinfo
  :straight nil
  :hook
  (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-echo
  :straight nil
  :hook
  (corfu-mode . corfu-echo-mode)
  :config
  (setq corfu-echo-delay 0))

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

;;;
;;; Text Editing
;;;

(use-package consult-flyspell)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

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
(use-package auto-yasnippet
  :bind
  (("<S-tab>" . aya-create)
   ("C-<tab>" . aya-expand)))
(use-package consult-yasnippet
  :bind
  (("C-M-/" . consult-yasnippet)))

;;;
;;; General Programming
;;;

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

(use-package magit)
(use-package consult-eglot)
(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))
(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;;;
;;; Lisp
;;;

(defconst init-lisp-modes
  '(emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-mode
    scheme-mode
    geiser-repl-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    cider-repl-mode))

(defun init-list-mode->hook (mode)
  "Derive a hook variable name from `MODE'."
  (declare (pure t) (side-effect-free t))
  (let ((mode-name (cond
                    ((symbolp mode) (symbol-name mode))
                    ((stringp mode) mode)
                    (t (signal 'wrong-type-argument '(stringp mode))))))
    (intern (format "%s-hook" mode-name))))

(defun init-lisp-hooks (&optional lisp-modes)
  "Return symbols refering to hook vars derrived from mode names.

          If `LISP-MODES' is given, this list will be converted.  Otherwise,
          `init-lisp-modes' will be used."
  (declare (pure t) (side-effect-free t))
  (mapcar #'init-list-mode->hook
          (or lisp-modes
              init-lisp-modes)))

(use-package paredit)

(dolist (hook (init-lisp-hooks))
  (declare-function enable-paredit-mode "paredit" ())
  (add-hook hook #'enable-paredit-mode)
  (add-hook hook #'show-paren-mode))

(provide 'init)
;;; init.el ends here
