;;; init.el -*- lexical-binding: t -*-

(eval-and-compile
  (require 'cl-lib))

;;;
;;; System Information
;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-WSL     (and IS-LINUX
              (string-match-p "Microsoft"
                      (shell-command-to-string "uname -a"))))

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

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (setq use-package-always-defer t
        use-package-hook-name-suffix nil))

;;;
;;; General Setup
;;;

(use-package emacs
  :custom
  (user-full-name "Jake Shilling")
  (user-mail-address "shilling.jake@gmail.com")
  :init
  (setq native-comp-jit-compilation nil)
  (setq custom-file nil)

  (let ((encoding (if IS-WINDOWS
                      'utf-8-dos
                    'utf-8-unix)))
    (cl-loop for fn in '(set-default-coding-systems
                         prefer-coding-system
                         set-terminal-coding-system
                         set-keyboard-coding-system
                         set-buffer-file-coding-system
                         set-selection-coding-system)
             do (apply fn (list encoding))))
  (set-language-environment "English")

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4))

(use-package delsel
  :commands
  delete-selection-mode
  :init
  (delete-selection-mode +1))

(use-package autorevert
  :commands
  global-auto-revert-mode
  :init
  (global-auto-revert-mode +1)
  :custom
  (global-auto-revert-non-file-buffers t))

(use-package whitespace
  :commands
  whitespace-mode
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode-hook . (lambda () (whitespace-mode +1)))
  (text-mode-hook . (lambda () (whitespace-mode -1))))

;;;
;;; Backups and Recovery
;;;

(setq auto-save-list-file-prefix
      (file-name-as-directory
       (init-cache-path "autosave")))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq backup-directory-alist
      `(("." . ,(init-cache-path "backups"))))

(setq make-backup-files t
      vc-make-backup-files nil
      backup-by-copying t
      version-control t
      kept-old-versions 6
      kept-new-versions 9
      delete-by-moving-to-trash nil)

;;;
;;; Bookmarks and Recentf
;;;

(use-package bookmark
  :custom
  (bookmark-default-file (init-cache-path "bookmark")))

(use-package recentf
  :commands
  recentf-mode
  :init
  (recentf-mode +1)
  :hook
  (recentf-mode-hook . (lambda ()
                         (run-with-idle-timer 30 t 'recentf-save-list)))
  :config
  (setq recentf-max-menu-items 50
        recentf-save-file (init-cache-path "recentf")))

;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :commands
  savehist-mode
  :init
  (savehist-mode +1)
  :config
  (setq kill-ring-max 50
        kill-do-not-save-duplicates t
        history-length 50
        savehist-file (init-cache-path "history"))
  (setq savehist-additional-variables
        '(kill-ring
          command-history
          set-variable-value-history
          query-replace-history
          read-expression-history
          minibuffer-history
          read-char-history
          face-name-history
          bookmark-history
          file-name-history))
  (put 'minibuffer-history 'history-length 50)
  (put 'file-name-history 'history-length 50)
  (put 'set-variable-value-history 'history-length 25)
  (put 'query-replace-history 'history-length 25)
  (put 'read-expression-history 'history-length 25)
  (put 'read-char-history 'history-length 25)
  (put 'face-name-history 'history-length 25)
  (put 'bookmark-history 'history-length 25)
  (setq history-delete-duplicates t))

(use-package saveplace
  :commands
  save-place-mode
  :init
  (save-place-mode +1)
  :config
  (setq save-place-file (init-cache-path "places")
        save-place-forget-unreadable-files t))

;;;
;;; Prompts
;;;

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;;
;;; Long Lines and Large Files
;;;

(setq vc-follow-symlinks t)

(setq large-file-warning-threshold nil)

(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

(use-package so-long
  :demand
  :init
  (global-so-long-mode +1))

;;;
;;; Help
;;;

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

;;;
;;; Spelling
;;;

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

;;;
;;; Managing Windows
;;;

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-ignore-current nil))

;;;
;;; Dired
;;;

(use-package nerd-icons-dired
  :commands
  nerd-icons-dired-mode
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

;;;
;;; IBuffer
;;;

(use-package ibuffer
  :commands
  ibuffer
  :bind
  (("C-x C-b" . ibuffer)))

(use-package nerd-icons-ibuffer
  :commands
  nerd-icons-ibuffer-mode
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)
(setq require-final-newline t)

(when IS-WSL
  ; WSLg breaks copy-paste from Emacs into Windows
  ; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
  (setq select-active-regions nil
        select-enable-clipboard 't
        select-enable-primary nil
        interprogram-cut-function #'gui-select-text))

(use-package vterm
  :commands
  vterm
  :bind ("C-c o t" . vterm))

;;;
;;; Appearance
;;;

(use-package modus-themes
  :demand
  :config
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t
        modus-themes-fringes nil)
  :init
  (load-theme 'modus-vivendi t))

(use-package pixel-scroll
  :demand
  :config
  (pixel-scroll-precision-mode +1))

(use-package display-line-numbers
  :hook
  (prog-mode-hook . (lambda () (display-line-numbers-mode +1)))
  (text-mode-hook . (lambda () (display-line-numbers-mode -1))))

;;;
;;; Completion
;;;

(setq completion-cycle-threshold nil)
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq minibuffer-prompt-properties
      '(readonly t cursor-intagible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((project-file (styles (partial-completion basic orderless)))
     (file (partial-completion basic orderless)))))

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

  :hook (completion-list-mode-hook . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package vertico
  :commands
  vertico-mode
  :init
  (vertico-mode +1))

(use-package marginalia
  :commands
  marginalia-mode
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode +1))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode +1)
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package vertico-multiform
  :hook (vertico-mode-hook . vertico-multiform-mode)
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

(use-package corfu
  :command
  global-cofu-mode
  :init
  (global-corfu-mode +1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package minibuffer
  :bind
  ("C-M-i" . completion-at-point))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

(use-package yasnippet
  :hook
  (yas-minor-mode-hook . (lambda ()
                           (setq-local hippie-expand-try-functions-list
                                       (cons #'yas-hippie-try-expand hippie-expand-try-functions-list)))))

;;;
;;; General Programming
;;;

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc--echo-area-prefer-doc-buffer-p t))

(use-package smartparens
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-n" . sp-up-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("C-)"   . sp-forward-slurp-sexp)
        ("C-("   . sp-backward-slurp-sexp)
        ("C-}"   . sp-forward-barf-sexp)
        ("C-{"   . sp-backward-barf-sexp)
        ("M-r"   . sp-raise-sexp)
        ("M-<up>" . sp-splice-sexp-killing-backward)
        ("M-<down>" . sp-splice-sexp-killing-forward)
        ("M-s"   . sp-splice-sexp)
        ("M-S"   . sp-split-sexp)
        ("M-J"   . sp-join-sexp))
  :config
  (sp-with-modes '(emacs-lisp-mode
                   ielm-mode
                   lisp-mode
                   lisp-data-mode
                   geiser-repl-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
  :hook
  ((emacs-lisp-mode-hook . smartparens-strict-mode)
   (eval-expression-minibuffer-setup-hook . smartparens-strict-mode)
   (ielm-mode-hook . smartparens-strict-mode)
   (lisp-mode-hook . smartparens-strict-mode)
   (lisp-data-mode-hook . smartparens-strict-mode)
   (scheme-mode-hook . smartparens-strict-mode)
   (geiser-repl-mode-hook . smartparens-strict-mode)
   (clojure-mode-hook . smartparens-strict-mode)
   (clojurec-mode-hook . smartparens-strict-mode)
   (clojurescope-mode-hook . smartparens-strict-mode)
   (cider-repl-mode-hook . smartparens-strict-mode)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((js-json-mode json-mode json-ts-mode jsonc-mode)
                 .
                 ("npx" "vscode-json-languageserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((yaml-ts-mode yaml-mode)
                 .
                 ("npx" "yaml-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode)
                 .
                 ("npx" "-p" "dockerfile-language-server-nodejs" "docker-langserver" "--stdio")))
  :commands
  eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-doc-buffer)))

(use-package flymake
  :commands
  flymake-mode
  :hook
  (prog-mode-hook . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;;;
;;; Language Specific
;;;

;; Nix

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package typescript-ts-mode
  :mode (("\\.ts[m]?\\'" . typescript-ts-mode)
         ("\\.[jt]s[m]x?\\'" . tsx-ts-mode))
  :interpreter "ts-node"
  :commands
  typescript-ts-base-mode
  typescript-ts-mode
  tsx-ts-mode)
