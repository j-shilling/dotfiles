
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jake Shilling"
      user-mail-address "shilling.jake@gmail.com")

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


(require 'xdg)
(setq org-directory
      (expand-file-name "org"
                        (or
                         (xdg-user-dir "Documents")
                         (expand-file-name "Documents"
                                           (getenv "HOME")))))

(use-package! org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory)))

(require 'xdg)
(use-package! citar
  :custom
  (citar-bibliography `(,(expand-file-name "references.bib" (expand-file-name "bib"
                                                                   (or
                                                                    (xdg-user-dir "Documents")
                                                                    (expand-file-name "Documents"
                                                                                      (getenv "HOME"))))))))

(use-package! auth-source-pass
  :hook '(doom-first-input-hook . auth-source-pass-enable))

(use-package! pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook '(doom-first-input-hook . pixel-scroll-precision-mode))

(use-package! display-line-numbers
  :diminish display-line-numbers-mode
  :custom (display-line-numbers-type t)
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
  :hook
  (doom-first-input-hook . global-auto-revert-mode))

(use-package! subword
  :diminish subword-mode
  :hook
  (prog-mode-hook . subword-mode))

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
  (dired-mode-hook . dired-omit-mode)
  (dired-mode-hook . dired-hide-details-mode))

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
                                    "SSH_AUTH_SOCK"
                                    "ANTHROPIC_API_KEY"))
  :hook
  (doom-first-input-hook . exec-path-from-shell-initialize))

(use-package! flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(when (modulep! :tools lsp -eglot)
  (use-package! lsp-mode
    ;; ... previous configuration
    :preface
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)

         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    :init
    (setq lsp-use-plists t)
    ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

  (use-package!
      lsp-tailwindcss :after lsp-mode))

(when (modulep! :tools lsp +eglot)
  (after! eglot
    (add-to-list 'eglot-server-programs
                 `(((js-mode :language-id "javascript")
                    (js-ts-mode :language-id "javascript")
                    (tsx-ts-mode :language-id "typescriptreact")
                    (typescript-ts-mode :language-id "typescript")
                    (typescript-tsx-mode :language-id "typescript")
                    (typescript-mode :language-id "typescript"))
                   . ("typescript-language-server" "--stdio"))))

  (use-package! eglot-booster
    :if (modulep! :tools lsp +eglot)
    :after eglot
    :config (eglot-booster-mode)))

(after! devdocs
  (bind-key "C-h D" #'devdocs-lookup))

(use-package! gptel
 :init
 (setq
  gptel-model 'claude-sonnet-4-20250514
  gptel-backend (gptel-make-anthropic "Claude"
                  :stream t :key (lambda ()
                                  (+pass-get-secret "FunctorFactory/anthropic-api-key")))))

(use-package! claude-code
  :bind-keymap
  ("C-c o c" . claude-code-command-map)
  :custom
  (claude-code-terminal-backend 'vterm)
  :hook
  (claude-code-process-environment-functions . monet-start-server-function)
  (doom-first-input-hook . claude-code-mode))

(use-package! monet
  :after claude-code
  :hook
  (claude-code-process-environment-functions . monet-start-server-function)
  (doom-first-input-hook . monet-mode)
  :custom
  (monet-ediff-split-window-direction 'vertical))
