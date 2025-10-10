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
  :hook
  ((dired-mode-hook . dired-omit-mode)
   (dired-mode-hook . dired-hide-details-mode))
  :config
  (if (init-lib-mac-p)
      (when-let ((gls (executable-find "gls")))
        (setopt dired-use-ls-dired t
                insert-directory-program gls
                dired-listing-switches "-aBhl  --group-directories-first"))
    (setopt dired-use-ls-dired t
            dired-listing-switches "-aBhl  --group-directories-first")))

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

;;; init-tools.el ends here.
