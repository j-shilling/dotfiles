;;; Init.el --- Initialization -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-WSL     (and IS-LINUX
                          (string-match-p "Microsoft"
                                          (shell-command-to-string "uname -a"))))

(defun init--parts-to-path (&rest args)
  (require 'seq)
  (seq-reduce (lambda (acc part)
                (expand-file-name part acc))
              args
              default-directory))

(defun init--state-file (&rest args)
  (require 'xdg)
  (declare-function xdg-state-home 'xdg)
  (apply #'init--parts-to-path (xdg-state-home) "emacs" emacs-version args))

(defun init--cache-file (&rest args)
  (require 'xdg)
  (declare-function xdg-cache-home 'xdg)
  (apply #'init--parts-to-path (xdg-cache-home) "emacs" emacs-version args))

;;;
;;; Packages
;;;

(require 'package)

(setopt package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-user-dir (init--state-file "elpa")
        package-quickstart-file (init--state-file "package-quickstart.el")
        package-quickstart t
        package-install-upgrade-built-in t
        package-selected-packages '(which-key
                                    vertico
                                    orderless
                                    marginalia
                                    diminish
                                    consult
                                    corfu
                                    embark
                                    embark-consult
                                    terraform-mode
                                    envrc
                                    exec-path-from-shell
                                    consult-eglot
                                    consult-eglot-embark
                                    multiple-cursors
                                    pass
                                    wgrep
                                    diff-hl
                                    ibuffer
                                    magit
                                    magit-todos
                                    forge
                                    helpful
                                    devdocs
                                    apheleia
                                    smartparens
                                    all-the-icons
                                    all-the-icons-dired
                                    all-the-icons-ibuffer
                                    all-the-icons-completion
                                    ace-window
                                    avy
                                    rbenv
                                    gptel
                                    mcp))

(package-initialize)

(setopt use-package-enable-imenu-support t
        use-package-hook-name-suffix nil)

(eval-when-compile
  (require 'use-package))

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;;
;;; General
;;;

(use-package emacs
  :custom
  (custom-file (init--state-file "custom.el"))
  (user-full-name    "Jake Shilling")
  (user-mail-address "shilling.jake@gmail.com")
  (use-short-answers  t)
  (ring-bell-function #'ignore)

  (load-prefer-newer t)

  (delete-by-moving-to-trash nil)

  (kill-ring-max 120)
  (kill-do-not-save-duplicates t)

  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

  (vc-follow-symlinks t)
  ;; Don't warn because VLF will
  (large-file-warning-threshold nil)

  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)

  (require-final-newline t)

  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)

  (sentence-end-double-space nil)

  (ecomplete-database-file          (init--cache-file "ecomplete-database.el"))

  (ede-project-placeholder-cache-file (init--cache-file "ede-projects.el"))
  (erc-dcc-get-default-directory    (init--cache-file "erc/dcc/"))
  (erc-log-channels-directory       (init--cache-file "erc/log-channels/"))
  (erc-startup-file-list            (list (init--state-file "erc/startup.el") (init--state-file "erc/startup") ".ercrc.el" ".ercrc"))
  (eudc-options-file                (init--state-file "eudc-options.el"))
  (eww-bookmarks-directory          (init--cache-file "eww/"))
  (filesets-menu-cache-file         (init--cache-file "filesets-menu-cache.el"))
  (gamegrid-user-score-file-directory (init--cache-file "gamegrid-user-score/"))
  (gnus-dribble-directory           (init--cache-file "gnus/dribble/"))
  (gnus-init-file                   (init--state-file "gnus/init.el"))
  ;; Gnus hardcodes newsrc.eld to be based on gnus-startup-file.
  (gnus-startup-file                (init--state-file "gnus/newsrc"))
  (ido-save-directory-list-file     (init--cache-file "ido-save-directory-list.el"))
  (ielm-history-file-name           (init--cache-file "ielm-history.eld"))
  (image-dired-db-file              (init--cache-file "image-dired/db.el"))
  (image-dired-dir                  (init--cache-file "image-dired/"))
  (image-dired-gallery-dir          (init--cache-file "image-dired/gallery/"))
  (image-dired-temp-image-file      (init--cache-file "image-dired/temp-image"))
  (image-dired-temp-rotate-image-file (init--cache-file "image-dired/temp-rotate-image"))
  (Info-saved-history-file          (init--cache-file "info-saved-history.eld"))
  (kkc-init-file-name               (init--cache-file "kkc-init.el"))
  (multisession-directory           (init--cache-file "multisession/"))
  (newsticker-cache-filename        (init--cache-file "newsticker/cache.el"))
  (newsticker-dir                   (init--cache-file "newsticker/data/"))
  (nsm-settings-file                (init--cache-file "nsm-settings.el"))
  (org-clock-persist-file           (init--cache-file "org/clock-persist.el"))
  (org-id-locations-file            (init--cache-file "org/id-locations.el"))
  (org-persist-directory            (init--cache-file "org/persist/"))
  (org-publish-timestamp-directory  (init--cache-file "org/timestamps/"))
  (persist--directory-location      (init--cache-file "persist/"))
  (project-list-file                (init--cache-file "project-list.el"))
  (quickurl-url-file                (init--cache-file "quickurl-url.el"))
  (rcirc-log-directory              (init--cache-file "rcirc-log/"))
  (recentf-save-file                (init--cache-file "recentf-save.el"))
  (remember-data-directory          (init--cache-file "remember/data.d/"))
  (remember-data-file               (init--cache-file "remember/data"))
  (save-place-file                  (init--cache-file "save-place.el"))
  (savehist-file                    (init--cache-file "savehist.el"))
  (semanticdb-default-save-directory (init--cache-file "semantic/"))
  (shadow-info-file                 (init--cache-file "shadow/info.el"))
  (shadow-todo-file                 (init--cache-file "shadow/todo.el"))
  (shared-game-score-directory      (init--cache-file "shared-game-score/"))
  (srecode-map-save-file            (init--cache-file "srecode-map.el"))
  (timeclock-file                   (init--cache-file "timeclock"))
  (tramp-auto-save-directory        (init--cache-file "tramp/auto-save/"))
  (tramp-persistency-file-name      (init--cache-file "tramp/persistency.el"))
  (type-break-file-name             (init--cache-file "type-break.el"))
  (url-cache-directory              (init--cache-file "url/cache/"))
  (url-configuration-directory      (init--cache-file "url/"))
  (url-cookie-file                  (init--cache-file "url/cookies.el"))
  (url-history-file                 (init--cache-file "url/history.el"))

  :init
  (setq native-comp-jit-compilation nil)

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
  (setq-default tab-width 4)

  (when IS-WSL
    ;; WSLg breaks copy-paste from Emacs into Windows
    ;; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
    (setq select-active-regions nil
          select-enable-clipboard 't
          select-enable-primary nil
          interprogram-cut-function #'gui-select-text)))

(use-package emacs
  :if IS-MAC
  :preface
  (defun init-macify ()
    (interactive)
    (setopt ns-option-modifier 'super
            ns-command-modifier 'meta))
  (defun init-unmacify ()
    (interactive)
    (setopt ns-option-modifier 'meta
            ns-command-modifier 'super))
  :hook
  ((after-init-hook . init-macify)))

;;;
;;; Appearance
;;;

(use-package emacs
  :init
  (set-default 'cursor-type '(bar . 1))
  (setq-default cursor-in-non-selected-windows nil)
  (set-frame-parameter (selected-frame) 'internal-border-width 8)
  (setq window-divider-default-right-width 8)

  (when IS-MAC
    (set-frame-font "JetBrains Mono 16")
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  :hook
  ((after-init-hook . window-divider-mode)))

(use-package menu-bar
  :config
  (menu-bar-mode 0))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package fringe
  :config
  (set-fringe-mode 8))

(use-package fontset
  :init
  (setq use-default-font-for-symbols nil)
  :config
  (set-fontset-font t 'symbol "Noto Emoji" nil 'append)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  (set-fontset-font "fontset-default" nil
                    (font-spec :name "Noto Emoji")))

(use-package all-the-icons
  :if (package-installed-p 'all-the-icons)
  :commands (all-the-icons-insert all-the-icons-install-fonts))

(use-package all-the-icons-dired
  :if (package-installed-p 'all-the-icons-dired)
  :custom
  (all-the-icons-dired-monochrome nil)
  :hook
  ((dired-mode-hook . all-the-icons-dired-mode)))

(use-package all-the-icons-ibuffer
  :if (package-installed-p 'all-the-icons-ibuffer)
  :hook
  ((ibuffer-mode-hook . all-the-icons-ibuffer-mode)))

(use-package all-the-icons-completion
  :if (and (package-installed-p 'all-the-icons-completion)
           (package-installed-p 'marginalia))
  :hook
  ((marginalia-mode-hook . all-the-icons-completion-marginalia-setup)))

(use-package all-the-icons-completion
  :if (and (package-installed-p 'all-the-icons-completion)
           (not (package-installed-p 'marginalia)))
  :hook
  ((after-init-hook . all-the-icons-completion-mode)))

(use-package pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook
  ((after-init-hook . pixel-scroll-precision-mode)))

(use-package display-line-numbers
  :diminish display-line-numbers-mode
  :preface
  (defun init-display-line-numbers-mode-on ()
    "Turn on display-line-numbers-mode."
    (display-line-numbers-mode +1))
  (defun init-display-line-numbers-mode-off ()
    "Turn off display-line-numbers-mode."
    (display-line-numbers-mode -1))
  :hook
  (prog-mode-hook init-display-line-numbers-mode-on)
  (text-mode-hook init-display-line-numbers-mode-off))

(use-package whitespace
  :diminish whitespace-mode
  :preface
  (defun init-whitespace-mode-on ()
    "Turn on whitespace-mode."
    (whitespace-mode +1))
  (defun init-whitespace-mode-off ()
    "Turn off whitespace-mode."
    (whitespace-mode -1))
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode-hook whitespace-mode-on)
  (text-mode-hook whitespace-mode-off))

(use-package modus-themes
  :defines
  modus-themes-mode-line
  modus-themes-diffs
  modus-themes-deuteranopia
  modus-themes-fringes
  :init
  (load-theme 'modus-vivendi t)
  :config
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t))

(use-package which-key
  :if (package-installed-p 'which-key)
  :diminish which-key-mode
  :hook
  (after-init-hook . which-key-mode))

(use-package prog-mode
  :hook
  (prog-mode-hook . prettify-symbols-mode))

;;;
;;; Auto Save / History
;;;

(use-package emacs
  :custom
  (auto-save-default  t)
  (auto-save-timeout  20)
  (auto-save-interval 200)
  (auto-save-list-file-prefix (init--cache-file "auto-save-list" ".saves-")))

(use-package desktop
  :custom
  (desktop-path (init--cache-file "desktop/")))

(use-package diary-lib
  :custom
  (diary-file (init--cache-file "diary")))

(use-package emacs
  :custom
  (make-backup-files    t)
  (vc-make-backup-files nil)
  (backup-by-copying    t)
  (version-control      t)
  (kept-old-versions    6)
  (kept-new-versions    9)
  (delete-old-versions  t))

(use-package calc
  :custom
  (calc-settings-file (init--state-file "calc-settings.el")))

(use-package abbrev
  :custom
  (abbrev-file-name (init--state-file "abbrev.el")))

(use-package autoinsert
  :custom
  (auto-insert-directory (init--state-file "auto-insert/")))

(use-package bookmark
  :custom
  (bookmark-default-file (init--cache-file "bookmark-default.el")))

(use-package eshell
  :custom
  (eshell-aliases-file (init--state-file "eshell" "aliases"))
  (eshell-directory-name (init--cache-file "eshell"))
  (eshell-login-script (init--state-file "eshell" "login"))
  (eshell-rc-script (init--state-file "eshell" "rc"))
  (eshell-history-file-name (init--cache-file "eshell" "history"))
  (eshell-modules-list '(eshell-alias
                         eshell-banner
                         eshell-cmpl
                         eshell-dirs
                         eshell-elecslash
                         eshell-extpipe
                         eshell-glob
                         eshell-hist
                         eshell-ls
                         eshell-prompt
                         eshell-script
                         eshell-smart
                         eshell-tramp
                         eshell-unix
                         eshell-xtra))
  (eshell-visual-commands '("pnpm"
                            "yarn"
                            "npx"
                            "flatpak"
                            "docker"
                            "docker-compose"
                            "devcontainer"
                            "guix"
                            "terraform"))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t))

(use-package recentf
  :diminish recentf-mode
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 300)
  :hook
  ((after-init-hook . recentf-mode)))

(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :diminish savehist-mode
  :defines savehist-minibuffer-history-variables
  :custom
  (savehist-additional-variables '(kill-ring
                                   command-history
                                   set-variable-value-history
                                   query-replace-history
                                   read-expression-history
                                   minibuffer-history
                                   read-char-history
                                   face-name-history
                                   bookmark-history
                                   file-name-history))
  :hook
  (after-init-hook savehist-mode))

(use-package saveplace
  :diminish save-place-mode
  :custom
  (save-place-forget-unreadable-files t)
  :hook
  ((after-init-hook . save-place-mode)))

;;;
;;; Completion
;;;

(use-package minibuffer
  :custom
  (completion-cycle-threshold nil)
  (enable-recursive-minibuffers t)
  (tab-always-indent 'complete)
  (minibuffer-prompt-properties
   '(readonly t cursor-intagible t face minibuffer-prompt))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package icomplete
  :unless (package-installed-p 'vertico)
  :hook
  ((after-init-hook . fido-mode)))

(use-package vertico
  :if (package-installed-p 'vertico)
  :diminish vertico-mode
  :hook
  ((after-init-hook . vertico-mode)))

(use-package vertico-multiform
  :if (package-installed-p 'vertico)
  :diminish vertico-multiform-mode
  :defines (vertico-multiform-categories vertico-multiform-commands)
  :hook (vertico-mode-hook vertico-multiform-mode)
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

(use-package orderless
  :if (package-installed-p 'orderless)
  :hook
  ((after-init-hook .
                    (lambda (&rest _)
                      (require 'orderless)
                      (setq completion-styles '(orderless basic))
                      (setq completion-category-overrides
                            '((project-file (styles . (partial-completion basic orderless)))
                              (file (styles . (partial-completion basic orderless)))))))))

(use-package marginalia
  :if (package-installed-p 'marginalia)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook
  ((after-init-hook . marginalia-mode)))

(use-package consult
  :if (package-installed-p 'consult)
  :functions (consult-register-window consult-register-format)
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
         ;; M-s bindings (search-map)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-ripgrep)
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

  :hook (completion-list-mode-hook consult-preview-at-point-mode)

  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-imenu
  :if (package-installed-p 'consult)
  :bind
  (("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

(use-package corfu
  :if (package-installed-p 'corfu)
  :diminish global-corfu-mode
  :custom
  (corfu-auto t)
  (corfu-quite-no-match 'separator)
  :hook
  ((after-init-hook . global-corfu-mode)))

(use-package corfu-history
  :if (package-installed-p 'corfu)
  :diminish corfu-history-mode
  :hook
  ((corfu-mode-hook . corfu-history-mode)))

(use-package corfu-info
  :if (package-installed-p 'corfu)
  :defines corfu-mode-map
  :bind
  (:map corfu-mode-map
        ("M-g" . corfu-info-location)
        ("M-h" . corfu-info-documentation)))

(use-package corfu-popupinfo
  :if (package-installed-p 'corfu)
  :diminish corfu-popupinfo-mode
  :hook
  (corfu-mode-hook corfu-popupinfo-mode))

(use-package embark
  :if (package-installed-p 'embark)
  :autoload embark-prefix-help-command
  :bind
  (("C-." . embark-act)
   ("C-;". embark-dwim)
   ("C-h B" . embark-become))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :if (package-installed-p 'embark-consult)
  :hook
  (embark-collect-mode-hook consult-preview-at-point-mode))

;;;
;;; Editing
;;;

(use-package autorevert
  :hook
  ((after-init-hook . auto-revert-mode)))

(use-package multiple-cursors
  :if (package-installed-p 'multiple-cursors)
  :commands mc/sort-regions
  :custom
  (mc/list-file (init--cache-file ".mc-lists.el"))
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package avy
  :if (package-installed-p 'avy)
  :bind
  (("C-;" . 'avy-goto-char)
   ("C-'" . 'avy-goto-char-2)
   ("M-g f" . 'avy-goto-line)
   ("M-g w" . 'avy-goto-word-1)
   ("M-g e" . 'avy-goto-word-0)
   ("C-c C-j" . 'avy-resume)))

(use-package ace-window
  :if (package-installed-p 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("M-o" . 'ace-window)))

(use-package subword
  :diminish subword-mode
  :hook
  (prog-mode-hook subword-mode))

(use-package wgrep
  :if (package-installed-p 'wgrep)
  :demand t)

(use-package editorconfig
  :hook
  (prog-mode-hook editorconfig-mode))

;;;
;;; Org
;;;

(use-package org
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-adapt-indentation nil)
  (org-startup-indented nil)
  (org-ellipsis "⤵")
  (org-hide-emphasis-markers t)
  (org-log-into-drawer t)
  (org-default-notes-file (concat org-directory "/todo.org")))

(use-package org-src
  :custom
  (org-edit-src-content-indentation 0))

(use-package org-refile
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'full-file-path)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets `((nil . (:maxlevel . 3))
                        (org-agenda-files . (:maxlevel . 3)))))

(use-package org-id
  :custom
  (org-id-locations-file (concat (xdg-cache-home) "/emacs/org-id-locations")))

(use-package org-capture
  :bind
  (:map mode-specific-map
        ("c" . org-capture)))

;;;
;;; Tools
;;;

(use-package envrc
  :if (package-installed-p 'envrc)
  :diminish
  :hook
  ((after-init-hook . envrc-global-mode)))

(use-package exec-path-from-shell
  :if (package-installed-p 'exec-path-from-shell)
  :hook
  ((after-init-hook . exec-path-from-shell-initialize)))

(use-package auth-source-pass
  :hook
  ((after-init-hook . auth-source-pass-enable)))

(use-package grep
  :autoload grep-apply-setting
  :config
  (grep-apply-setting
   'grep-command
   "rg -n -H --no-heading ")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-listing-switches "-lah -v --group-directories-first")
  :hook
  ((dired-mode-hook . dired-omit-mode)
   (dired-mode-hook . dired-hide-details-mode)))

(use-package diff-hl
  :if (package-installed-p 'diff-hl)
  :diminish (diff-hl-mode diff-hl-dir-mode)
  :hook
  ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (prog-mode-hook . diff-hl-mode)
   (vc-dir-mode . diff-hl-dir-mode)))

(use-package diff-hl-dired
  :if (package-installed-p 'diff-hl)
  :diminish diff-hl-dired-mode
  :hook (dired-mode-hook diff-hl-dired-mode))

(use-package ibuffer
  :if (package-installed-p 'ibuffer)
  :bind
  (("C-x C-b" . ibuffer)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc--echo-area-prefer-doc-buffer-p t))

(use-package magit
  :if (package-installed-p 'magit)
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :if (package-installed-p 'magit-todos)
  :functions magit-todos-mode
  :after magit
  :commands magit-todos-list
  :config (magit-todos-mode +1))

(use-package forge
  :if (package-installed-p 'forge)
  :after magit)

(use-package helpful
  :if (package-installed-p 'helpful)
  :bind
  (("C-h f" . helpful-callable)
   ("C-h F" . helpful-function)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)))

(use-package devdocs
  :if (package-installed-p 'devdocs)
  :bind
  (("C-h D" . devdocs-lookup)))

(use-package emacs
  :custom
  (epa-pinentry-mode 'loopback))

(use-package transient
  :custom
  (transient-history-file (init--cache-file "transient" "history.el")))

(use-package project
  :custom
  (project-list-buffers #'project-list-buffers-ibuffer)
  (project-list-file (init--state-file "project"))
  (project-switch-commands `((project-find-file "Find file" "f")
                             (project-find-regexp "Find regexp" "g")
                             (project-find-dir "Find directory" "d")
                             (project-dired "Dired" "D")
                             (project-eshell "Eshell" "e")
                             ,(if (package-installed-p 'magit)
                                  '(magit-project-status "Magit" "v")
                                '(project-vc-dir "VC-Dir" "v"))
                             (project-any-command "Other" "o"))))

;;;
;;; Programming
;;;

(use-package apheleia
  :if (package-installed-p 'apheleia)
  :diminish apheleia-mode
  :hook
  ((after-init-hook . apheleia-global-mode)))

(use-package smartparens
  :if (package-installed-p 'smartparens)
  :diminish smart-parens-mode
  :defines smartparens-mode-map
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
  ;; load default configuration to setup other pairs
  (require 'smartparens-config)
  :hook
  ((emacs-lisp-mode-hook . smartparens-strict-mode)
   (eval-expression-minibuffer-setup-hook . smartparens-mode)
   (ielm-mode-hook . smartparens-strict-mode)
   (lisp-mode-hook . smartparens-strict-mode)
   (lisp-data-mode-hook . smartparens-strict-mode)
   (scheme-mode-hook . smartparens-strict-mode)
   (geiser-repl-mode-hook . smartparens-strict-mode)
   (clojure-mode-hook . smartparens-strict-mode)
   (clojurec-mode-hook . smartparens-strict-mode)
   (clojurescope-mode-hook . smartparens-strict-mode)
   (cider-repl-mode-hook . smartparens-strict-mode)
   (prog-mode-hook . smartparens-mode)
   (org-mode-hook . smartparens-mode)
   (markdown-mode-hook . smartparens-mode)))

(use-package consult-eglot-embark
  :if (and (package-installed-p 'consult-eglot-embark)
           (package-installed-p 'consult-eglot)
           (package-installed-p 'embark))
  :init
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult-eglot
      (require 'consult-eglot-embark)
      (consult-eglot-embark-mode))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 "npx" "typescript-language-server" "--stdio"))
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

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 .
                 ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(astro-mode
                 .
                 ("npx" "-p" "@astrojs/language-server" "astro-ls" "--stdio"
                  :initializationOptions
                  (:typescript (:tsdk "./node_modules/typescript/lib")))))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-doc-buffer)))

(use-package eglot-booster
  :if (package-installed-p 'eglot-booster)
  :functions eglot-booster-mode
  :after eglot
  :config (eglot-booster-mode))

(use-package consult-xref
  :if (package-installed-p 'consult)
  :autoload consult-xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package flymake
  :diminish flymake-mode
  :hook
  (eglot-mode-hook . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))



;;;
;;; Ruby
;;;

(use-package rbenv
  :if (package-installed-p 'rbenv)
  :hook ((after-init-hook . global-rbenv-mode)))

(use-package terraform-mode
  :if (package-installed-p 'terraform-mode)
  :mode ("\\.tf" "\\.tfvars" "\\.tfbackend")
  :hook
  ((terraform-mode-hook . eglot-ensure)))

(use-package js
  :if (executable-find "nodenv")
  :init
  (add-to-list 'exec-path (expand-file-name ".nodenv/shims" (getenv "HOME"))))

(load (expand-file-name "./init-tree-sitter.el" user-emacs-directory))
(load (expand-file-name "./init-ai.el" user-emacs-directory))

;;; init.el ends here
