;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defconst IS-WSL     (and (featurep :system 'linux)
                          (string-match-p "Microsoft"
                                          (shell-command-to-string "uname -a"))))

(when IS-WSL
    ;; WSLg breaks copy-paste from Emacs into Windows
    ;; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
    (setq select-active-regions nil
          select-enable-clipboard 't
          select-enable-primary nil
          interprogram-cut-function #'gui-select-text))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

(use-package! pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook
  (doom-first-input-hook . pixel-scroll-precision-mode))

(use-package! display-line-numbers
  :diminish display-line-numbers-mode
  :hook
  (prog-mode-hook . (lambda () (display-line-numbers-mode +1)))
  (text-mode-hook . (lambda () (display-line-numbers-mode -1))))

(use-package! whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode-hook . (lambda () (whitespace-mode +1)))
  (text-mode-hook . (lambda () (whitespace-mode -1))))

(use-package! autorevert
  :diminish global-auto-revert-mode
  :custom
  (global-auto-revert-non-file-buffers t)
  :hook (doom-first-input-hook . global-auto-revert-mode))

(use-package! subword
  :diminish subword-mode
  :hook
  (prog-mode-hook . subword-mode))

(use-package! pinentry
  :custom
  (epa-pinentry-mode 'loopback)
  :hook
  (after-init-hook . pinentry-start))

(use-package! dired
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

(use-package! proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package! exec-path-from-shell
  :unless (featurep :system 'windows)
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "SSH_AGENT_PID"
                                    "SSH_AUTH_SOCK"))
  :hook
  (after-init-hook . exec-path-from-shell-initialize))

(use-package! lsp)

(use-package! lsp-pyright
  :custom
  (lsp-pyright-langserver-command "basedpyright"))

(use-package! php-mode
  :custom
  (lsp-intelephense-server-command '("emacs-lsp-booster" "--" "intelephense" "--stdio"))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'"))
