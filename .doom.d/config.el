;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;
;;; Appearance
;;;

(setq modus-themes-mode-line '(borderless)
      modus-themes-diffs 'desaturated
      modus-themes-deuteranopia t
      modus-themes-fringes nil

      modus-themes-operandi-color-overrides
      '((fg-window-divider-inner . "#ffffff")
        (fg-window-divider-outer . "#ffffff"))
      modus-themes-vivendi-color-overrides
      '((fg-window-divider-inner . "#000000")
        (fg-window-divider-outer . "#000000")))

(setq doom-theme 'modus-vivendi)

(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16))

;;;
;;; Completion
;;;

(use-package! vertico-multiform
  :hook (vertico . vertico-multiform-mode)
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

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package! minibuffer
  :bind
  ("C-M-i" . completion-at-point))

(use-package! hippie-exp
  :bind
  ("M-/" . hippie-expand))

(use-package! yasnippet
  :hook
  (yas-minor-mode . (lambda ()
                      (setq-local hippie-expand-try-functions-list
                                  (cons #'yas-hippie-try-expand hippie-expand-try-functions-list)))))

(use-package! copilot
  :hook
  ((prog-mode . copilot-mode)
   (copilot-mode . (lambda ()
                     (setq-local hippie-expand-try-functions-list
                                 (cons #'copilot-accept-completion
                                       hippie-expand-try-functions-list))))))

;;;
;;; Tools
;;;

(use-package! epa
  :config
  (setq epa-pinentry-mode 'loopback
        epa-file-select-keys nil
        epa-file-encrypt-to "0FCC8E6A96FF109F"
        epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save t))

(use-package! grep
  :config
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(after! em-term
  (pushnew! eshell-visual-commands "pnpm" "yarn" "npm" "npx"))

(use-package! sql
  :custom
  (sql-connection-alist
   `((arena-analytics-portal
      (sql-product 'postgres)
      (sql-port 5555)
      (sql-server "localhost")
      (sql-database "arena_analytics_portal_db")
      (sql-user "postgres")
      (sql-password "postgres")))))

(use-package! exec-path-from-shell
  :custom
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "SSH_AUTH_SOCK"
     "PASSWORD_STORE_DIR"))
  :hook
  (doom-first-buffer . (lambda ()
                         (exec-path-from-shell-initialize)
                         (setq auth-source-pass-filename
                               (getenv "PASSWORD_STORE_DIR")))))

(use-package! auth-source-pass
  :custom
  (auth-source-pass-filename . (getenv "PASSWORD_STORE_DIR")))

(after! mu4e
  (setq mu4e-cha)
  (setq sendmail-program (executable-find "sendmail")
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(after! elfeed
  (setq rmh-elfeed-org-files
        (list (expand-file-name "elfeed.org" doom-user-dir))))

;;;
;;; General Programming
;;;

(use-package! eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc--echo-area-prefer-doc-buffer-p t))

(use-package! smartparens
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
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (eval-expression-minibuffer-setup . smartparens-strict-mode)
   (ielm-mode . smartparens-strict-mode)
   (lisp-mode . smartparens-strict-mode)
   (lisp-data-mode . smartparens-strict-mode)
   (scheme-mode . smartparens-strict-mode)
   (geiser-repl-mode . smartparens-strict-mode)
   (clojure-mode . smartparens-strict-mode)
   (clojurec-mode . smartparens-strict-mode)
   (clojurescope-mode . smartparens-strict-mode)
   (cider-repl-mode . smartparens-strict-mode)))

(use-package! eglot
  :config
  (add-to-list 'eglot-server-programs
               '(typescript-tsx-mode . ("typescript-language-server" "--stdio")))
  (setq-default eglot-workspace-configuration
                `(:typescript-language-server
                  (:maxTsServerMemory ,(* 1024 8))))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-doc-buffer)))

(use-package! flymake
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;;
;; Programming Language Specific
;;

(use-package! emacs-lisp-mode
  :hook
  ((emacs-lisp-mode . dash-fontify-mode)))

(use-package! ocamlformat
  :custom
  (ocamlformat-enable 'disable))

(use-package! utop
  :hook
  (utop . (lambda ()
            (setq-local company-idle-delay nil))))

(use-package! lisp-mode
  :config
  (let* ((roswell-dir (expand-file-name ".roswell" (getenv "HOME")))
         (helper.el (expand-file-name "helper.el" roswell-dir))
         (roswell-bin (expand-file-name "bin/" roswell-dir)))
    (when (file-exists-p helper.el)
      (load helper.el))
    (when (file-exists-p roswell-bin)
      (add-to-list 'exec-path roswell-bin)))
  ;; Load any files that CL packages might have installed
  (dolist (file '("~/.roswell/lisp/quicklisp/log4sly-setup.el"))
    (when (file-exists-p file)
      (load file)))
  (when (fboundp 'global-log4sly-mode)
    (global-log4sly-mode 1))
  (setq sly-lisp-implementations
        '((sbcl ("ros" "-L" "sbcl" "-Q" "-l" "~/.sbclrc" "run"))
          (ecl ("ros" "-L" "ecl" "-Q" "run")))))

(use-package! jest-test-mode
  :hook
  (typescript-mode js-mode js2-mode typescript-tsx-mode))

(defun init-prettier ()
  (setq-local +format-with-lsp nil)
  (setq-local +format-with 'prettier))
(dolist (hook '(typescript-mode-hook
                js-mode-hook
                js2-mode-hook
                typescript-tsx-mode-hook
                json-mode-hook
                yaml-mode-hook
                markdown-mode-hook))
  (add-hook hook #'init-prettier))

(use-package! add-node-modules-path
  :hook
  (prog-mode . add-node-modules-path))

(use-package! dap-mode
  :init
  (defun init-javascript ()
    (require 'dap-node nil t)
    (require 'dap-chrome nil t)

    (setq-local +format-with-lsp nil)
    (setq-local +format-with 'prettier))
  :hook
  ((typescript . init-javascript)
   (js-mode . init-javascript)
   (js2-mode . init-javascript)
   (typescript-ts-mode . init-javascript)))
