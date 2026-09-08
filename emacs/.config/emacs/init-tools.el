;;; init-tools.el --- Configure various emacs utilities  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;
;;; Shell / Term
;;;;;;;;;;;;;;;;;;

(use-package eshell
  :custom
  (eshell-aliases-file (init-lib-state-file "eshell" "aliases"))
  (eshell-directory-name (init-lib-cache-file "eshell"))
  (eshell-login-script (init-lib-state-file "eshell" "login"))
  (eshell-rc-script (init-lib-state-file "eshell" "rc"))
  (eshell-history-file-name (init-lib-cache-file "eshell" "history"))
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

(use-package eat
  :if (package-installed-p 'eat)
  :hook
  ((eshell-load-hook . eat-eshell-mode)
   (eshell-load-hook . eat-eshell-visual-command-mode)))

(use-package envrc
  :if (package-installed-p 'envrc)
  :diminish
  :hook
  ((after-init-hook . envrc-global-mode)))

(use-package exec-path-from-shell
  :if (package-installed-p 'exec-path-from-shell)
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "PASSWORD_STORE_DIR"
                                    "XDG_CONFIG_HOME"
                                    "XDG_DATA_HOME"
                                    "XDG_CACHE_HOME"
                                    "XDG_STATE_HOME"
                                    "SSH_AUTH_SOCK"))
  :hook
  ((after-init-hook . exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :hook
  ((dired-mode-hook . dired-hide-details-mode))
  :config
  (defvar init-tools--base-dired-listing-switches
    '("-A" ; show almost all, hidden files without . and ..
      "-h" ; human readable file sizes
      "-l" ; long format w/ permissions etc
      ))

  (defvar init-tools--gnu-dired-listing-switches
    '("--ignore-backups"
      "--group-directories-first"))

  (defun init-tools--config-dired-ls ()
  "Decides whether to use GNU ls extensions and configures affected dired
options."
  (let ((use-gnu-ls))
    (when (init-lib-linux-p)
      ;; Assume GNU on linux
      (setq use-gnu-ls t))
    (when (init-lib-mac-p)
      ;; Look for gls on mac
      (when-let* ((gls (executable-find "gls")))
        (setopt insert-directory-program gls)
        (setq use-gnu-ls t)))

    (setopt dired-listing-switches
            (string-join (if use-gnu-ls
                             `(,@init-tools--base-dired-listing-switches
                               ,@init-tools--gnu-dired-listing-switches)
                           init-tools--base-dired-listing-switches)
                         " "))
    (setopt dired-use-ls-dired use-gnu-ls)))

  (init-tools--config-dired-ls))

(use-package dired-x
  :after dired
  :hook
  ((dired-mode-hook . dired-omit-mode)))

(use-package all-the-icons-dired
  :if (package-installed-p 'all-the-icons-dired)
  :custom
  (all-the-icons-dired-monochrome nil)
  :hook
  ((dired-mode-hook . all-the-icons-dired-mode)))

(use-package diff-hl-dired
  :if (package-installed-p 'diff-hl)
  :diminish diff-hl-dired-mode
  :hook (dired-mode-hook diff-hl-dired-mode))

;;;;;;;;;;;;;;;;;;
;;; ediff
;;;;;;;;;;;;;;;;;;

(use-package ediff
  :custom
  (ediff-keep-variants t)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;
;;; proced
;;;;;;;;;;;;;;;;;;

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

;;;;;;;;;;;;;;;;;;
;;; grep
;;;;;;;;;;;;;;;;;;

(use-package wgrep
  :if (package-installed-p 'wgrep)
  :demand t)

(use-package grep
  :autoload grep-apply-setting
  :config
  (grep-apply-setting
   'grep-command
   "rg -n -H --no-heading ")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;;;;;;;;;;;;;;;;;;
;;; pass / gpg
;;;;;;;;;;;;;;;;;;

(use-package auth-source-pass
  :hook
  ((after-init-hook . auth-source-pass-enable))
  :custom
  (auth-source-pass-filename
   (or
    (if (package-installed-p 'exec-path-from-shell)
        (progn
        (require 'exec-path-from-shell)
        (exec-path-from-shell-getenv "PASSWORD_STORE_DIR"))
      (getenv "PASSWORD_STORE_DIR"))
    "~/.password-store")))

(use-package emacs
  :custom
  (epa-pinentry-mode 'loopback))

(use-package pass
  :if (package-installed-p 'pass))

;;;;;;;;;;;;;;;;;;
;;; git
;;;;;;;;;;;;;;;;;;

(use-package magit
  :if (package-installed-p 'magit)
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :if (package-installed-p 'magit-todos)
  :functions magit-todos-mode
  :after magit
  :commands magit-todos-list
  :config (magit-todos-mode +1))

(use-package gptel-magit
  :if (package-installed-p 'gptel-magit)
  :hook (magit-mode . gptel-magit-install))

(use-package forge
  :if (package-installed-p 'forge)
  :after magit)

(use-package gptel-forge-prs
  :after forge
  :if (package-installed-p 'gptel-forge-prs)
  :config
  (gptel-forge-prs-install))

(use-package diff-hl
  :if (package-installed-p 'diff-hl)
  :diminish (diff-hl-mode diff-hl-dir-mode)
  :hook
  ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (prog-mode-hook . diff-hl-mode)
   (vc-dir-mode . diff-hl-dir-mode)))

;;;;;;;;;;;;;;;;;;
;;; docs / help
;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc--echo-area-prefer-doc-buffer-p t))

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

;;;;;;;;;;;;;;;;;;
;;; misc
;;;;;;;;;;;;;;;;;;

(use-package ibuffer
  :if (package-installed-p 'ibuffer)
  :bind
  (("C-x C-b" . ibuffer)))

(use-package transient
  :custom
  (transient-history-file (init-lib-cache-file "transient" "history.el")))

(use-package xwidget
  :if (featurep 'xwidget-internal)
  :custom
  (browse-url-browser-function 'xwidget-webkit-browse-url))

(use-package makefile-executor
  :if (package-installed-p 'makefile-executor)
  :hook
  ((makefile-mode-hook . makefile-executor-mode)))

;;; init-tools.el ends here.
