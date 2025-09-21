;;; init.el --- Initialization -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;;;
;;; Packages
;;;

(require 'package)
(setopt package-archives '(("melpa" . "http://melpa.org/packages/")
                            ("org" . "http://orgmode.org/elpa/")
                            ("gnu" . "https://elpa.gnu.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defconst init--packages
  '()
  "External packages to install")

(defun init-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg init--packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

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
  (apply #'init--parts-to-path (xdg-state-home) "emacs" args))

(defun init--cache-file (&rest args)
  (require 'xdg)
  (apply #'init--parts-to-path (xdg-cache-home) "emacs" args))

(use-package emacs
  :custom
  (user-full-name    "Jake Shilling")
  (user-mail-address "shilling.jake@gmail.com")
  (use-short-answers  t)
  (ring-bell-function #'ignore)

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
  ;; (startup-redirect-eln-cache (init--cache-file "eln-cache"))
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
  (setq-default tab-width 4)

  (when IS-WSL
    ;; WSLg breaks copy-paste from Emacs into Windows
    ;; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
    (setq select-active-regions nil
          select-enable-clipboard 't
          select-enable-primary nil
          interprogram-cut-function #'gui-select-text)))

;;;
;;; Appearance
;;;

(use-package emacs
  :init
  (set-default 'cursor-type '(bar . 1))
  (setq-default cursor-in-non-selected-windows nil)
  (setq bookmark-set-fringe-mark nil)
  (set-frame-parameter (selected-frame) 'internal-border-width 8)
  (setq window-divider-default-right-width 8)
  :hook
  (after-init-hook . window-divider-mode))

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

(use-package pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package display-line-numbers
  :diminish display-line-numbers-mode
  :hook
  (prog-mode-hook . (lambda () (display-line-numbers-mode +1)))
  (text-mode-hook . (lambda () (display-line-numbers-mode -1))))

(use-package whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode-hook . (lambda () (whitespace-mode +1)))
  (text-mode-hook . (lambda () (whitespace-mode -1))))

(use-package modus-themes
  :defines
  modus-themes-mode-line
  modus-themes-diffs
  modus-themes-deuteranopia
  modus-themes-fringes
  :config
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t)
  (load-theme 'modus-vivendi t (not (display-graphic-p))))

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
  (kept-new-versions    9))

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
  (eshell-error-if-no-glob t)
  :hook
  (eshell-mode-hook . (lambda ()
                        (setenv "TERM" "xterm-256color"))))

(use-package recentf
  :diminish recentf-mode
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 300)
  :hook
  (after-init-hook recentf-mode))

(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :diminish savehist-mode
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
  (after-init-mode . savehist-mode))

(use-package saveplace
  :diminish save-place-mode
  :custom
  (save-place-forget-unreadable-files t)
  :hook
  (after-init-hook . save-place-mode))

;;;
;;; Completion
;;;

(setq completion-cycle-threshold nil)
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq minibuffer-prompt-properties
      '(readonly t cursor-intagible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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

(let*
    ((init-org-super-agenda-config
      `((org-super-agenda-unmatched-name 'none)
        (org-super-agenda-unmatched-order 5)
        (org-super-agenda-header-separator "\n")
        (org-super-agenda-groups
         `((:name "Clocked today"
                  :log t
                  :order 100)
           (:name none
                  :todo ("IDEA")
                  :order 1)
           (:name none
                  :todo ("PROJ")
                  :order 2)
           (:name none
                  :todo ,org-done-keywords-for-agenda
                  :order 10)))))
     (init-org-agenda-custom-commands
      `(((kbd "C-d") "Agenda for the day"
         ((agenda
           ""
           ((org-agenda-span 1)
            (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
            (org-agenda-block-separator nil)
            (org-scheduled-past-days 0)
            ,@init-org-super-agenda-config
            (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
            (org-agenda-format-date "%A %-e %B %Y")
            (org-agenda-overriding-header "\nAgenda for the day\n")))
          (todo
           "NEXT"
           ((org-agenda-block-separator nil)
            (org-agenda-overriding-header "\nCurrent Tasks\n")))))
        (list
         (kbd "C-o") "Overview"
         ((agenda
           "*"
           ((org-agenda-scheduled-leaders '("" "Sched. %2dx:"))
            ,@init-org-super-agenda-config
            (org-agenda-block-separator nil)
            (org-agenda-span 14)
            (org-agenda-show-future-repeats nil)
            (org-agenda-skip-deadline-prewarning-if-scheduled t)
            (org-agenda-overriding-header "\nAgenda\n")))
          (agenda
           ""
           ((org-agenda-start-on-weekday nil)
            (org-agenda-start-day "+1d")
            (org-agenda-span 14)
            (org-agenda-show-all-dates nil)
            (org-agenda-time-grid nil)
            (org-agenda-show-future-repeats nil)
            (org-agenda-block-separator nil)
            (org-agenda-entry-types '(:deadline))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
            (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
          (alltodo
           ""
           ((org-agenda-block-separator nil)
            (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled))))
            ,@init-org-super-agenda-config
            (org-agenda-overriding-header "\nBacklog\n"))))))))
  (use-package org-agenda
    :straight nil
    :ensure nil
    :custom
    (org-agenda-custom-commands init-org-agenda-custom-commands)
    (org-agenda-tags-column 0)
    (org-agenda-sticky t)
    (org-agenda-block-separator ?-)
    (org-agenda-time-grid '((daily today require-timed)
                            (800 1000 1200 1400 1600 1800 2000)
                            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (org-agenda-current-time-string
     "⭠ now ─────────────────────────────────────────────────")
    (org-agenda-start-with-log-mode t)
    (org-agenda-dim-blocked-tasks t)
    (org-agenda-skip-scheduled-if-done nil)
    (org-agenda-skip-deadline-if-done nil)
    (org-agenda-compact-blocks nil)
    (org-agenda-log-mode-add-notes nil)
    (org-agenda-bulk-custom-functions
     '((?P (lambda nil
             (org-agenda-priority 'set)))))))

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
  :straight nil
  :ensure nil
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

;;; init.el ends here
