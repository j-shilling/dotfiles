;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;
;;; Appearance
;;;

(setq modus-themes-mode-line '(borderless)
      modus-themes-diffs 'desaturated
      modus-themes-deuteranopia t
      modus-themes-fringes nil)

(setq doom-theme 'modus-vivendi)

(find-font (font-spec :name "Noto Mono"))

(defun first-available-font (font-names)
  "Return the first element of `FONT-NAMES' that can be found with `find-font'."
  (when-let ((font-name (cl-find-if (lambda (font)
                                      (find-font (font-spec :name font)))
                                    font-names)))
    (font-spec :name font-name :size 16)))

(setq doom-font (first-available-font '("Fira Code" "Noto Mono")))
(setq doom-variable-pitch-font (first-available-font '("Fira Sans" "Noto Sans")))

(use-package! whitespace
  :config
  (setq
    whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
    whitespace-display-mappings '(
      (space-mark   ?\     [?\u00B7]     [?.])
      (space-mark   ?\xA0  [?\u00A4]     [?_])
      (newline-mark ?\n    [182 ?\n])
      (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  (global-whitespace-mode +1))

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
  ((copilot-mode . (lambda ()
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

(use-package! em-term
  :custom
  (eshell-destroy-buffer-when-process-dies nil)
  :config
  (pushnew! eshell-visual-commands
            "pnpm"
            "yarn"
            "npm"
            "npx"
            "flatpak"
            "docker"
            "docker-compose"
            "devcontainer"))

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
  (setq mu4e-change-filenames-when-moving t
        mu4e-update-interval nil)

  (setq sendmail-program (executable-find "sendmail")
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "shilling.jake"
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/shilling.jake" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "shilling.jake@gmail.com")
                    (mu4e-sent-folder . "/shilling.jake/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/shilling.jake/[Gmail]/Drafts")
                    (mu4e-trash-folder . "/shilling.jake/[Gmail]/Trash")
                    (mu4e-refile-folder . "/shilling.jake/[Gmail]/All Mail"))))))

(after! elfeed
  (setq rmh-elfeed-org-files
        (list (expand-file-name "elfeed.org" doom-user-dir))))

;;;
;;; Org
;;;
(defun jethro/org-roam-node-from-cite (keys-entries)
  "https://jethrokuan.github.io/org-roam-guide/"
  (interactive (list (citar-select-ref)))
  (let ((title (citar-format--entry keys-entries
                                    "${author editor} :: ${title}")))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "reference/${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey keys-entries)
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

(setq org-directory (expand-file-name "org" (getenv "HOME")))
(setq org-roam-directory (expand-file-name "roam" org-directory))
(setq citar-bibliography (expand-file-name "library.bib" org-directory))

(when (file-exists-p "/usr/share/plantuml/plantuml.jar")
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(add-hook! 'text-mode-hook #'auto-fill-mode)

(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-deadline-warning-days 5

      org-agenda-todo-list-sublevels t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'all
      org-agenda-todo-ignore-with-date 'all
      org-agenda-tags-todo-honor-ignore-options t

      org-todo-keywords '((type "TODO(t@/!)" "WIP(w@/!)" "|" "DONE(d@/!)" "KILL(k@/!)"))

      org-log-into-drawer t
      org-log-done t
      org-log-reschedule t
      org-log-redeadline t

      org-enable-priority-commands nil

      org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago: ")
      org-agenda-scheduled-leaders '("DO: " "Sched.%2dx: "))

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
               '((typescript-tsx-mode :language-id "typescriptreact")
                 .
                 ("typescript-language-server" "--stdio")))
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

(use-package! utop
  :hook
  (utop . (lambda ()
            (setq-local company-idle-delay nil))))

(use-package! opam-switch-mode
  :hook
  (tuareg-mode . opam-switch-mode)
  :bind
  ([remap tuareg-opam-update-env] . opam-switch-set-switch))

(use-package! merlin
  :hook
  (tuareg-mode . merlin-mode))

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
  :custom
  (add-node-modules-path-command '("pnpm bin" "echo \"$(npm config get prefix)/bin\""))
  :hook
  (prog-mode . add-node-modules-path))

(use-package! dap-mode
  :init
  (defun init-javascript ()
    (copilot-mode 1))
  :hook
  ((typescript . init-javascript)
   (js-mode . init-javascript)
   (js2-mode . init-javascript)
   (typescript-ts-mode . init-javascript)))

(use-package! pyvenv
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))
