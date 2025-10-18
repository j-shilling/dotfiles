;;; init-prog.el --- Configuration for programming -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

;; Prog Mode

(use-package subword
    :diminish subword-mode
    :hook
    ((prog-mode-hook . subword-mode)))

(use-package editorconfig
    :hook
  ((prog-mode-hook . editorconfig-mode)))

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

(use-package eglot
    :custom
  (eglot-autoreconnect t)
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  (eglot-extend-to-xref t)
  (eglot-report-progress t)
  :config
  (add-to-list 'eglot-server-programs
               '((js-json-mode json-mode json-ts-mode jsonc-mode)
                 .
                 ("npx" "vscode-json-languageserver" "--stdio")))

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

  (setq-default eglot-workspace-configuration
                '(
                  :json ( :format (:enable nil)
                         :validate (:enable t)
                         :schemas (
                                   :fileMatch ["package.json"]
                                   :url "https://www.schemastore.org/package.json"))
                  :yaml ( :format (:enable t)
                         :validate t
                         :hover t
                         :completion t
                         :schemas (
                                   https://json.schemastore.org/yamllint.json ["/*.yml"])
                         :schemaStore (:enable t))))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-doc-buffer)))

(use-package consult-eglot-embark
    :if (and (package-installed-p 'consult-eglot-embark)
             (package-installed-p 'consult-eglot)
             (package-installed-p 'embark))
    :init
    (with-eval-after-load 'embark
      (with-eval-after-load 'consult-eglot
        (require 'consult-eglot-embark)
        (consult-eglot-embark-mode))))

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

;;; Lisp

(use-package lisp-mode
    :custom
  (lisp-indent-function 'common-lisp-indent-function))

;;; TypetScript / JavaScript

(use-package eglot
    :config
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 "npx" "typescript-language-server" "--stdio")))

(use-package js
    :mode (("\\.[cm]?js\\'" . js-ts-mode)
           ("\\.jsx\\'" . jsx-ts-mode))
    :hook
    ((js-base-mode-hook . eglot-ensure)))

(use-package typescript-ts-mode
    :mode (("\\.ts\\'" . typescript-ts-mode)
           ("\\.txs\\'" . tsx-ts-mode))
    :hook
    ((typescript-ts-base-mode-hook . eglot-ensure)))

;; Ruby

(use-package rbenv
    :if (package-installed-p 'rbenv)
    :hook ((after-init-hook . global-rbenv-mode))
    :custom
    (rbenv-executable (executable-find "rbenv")))

(use-package ruby-ts-mode
    :mode ("\\.rbw?"
           "\\.ru"
           "\\.rake"
           "\\.thor"
           "\\.axlsx"
           "\\.jbuilder"
           "\\.rabl"
           "\\.gemspec"
           "\\.podspec"
           "Gemfile"
           "Rakefile"
           "Capfile"
           "Thorfile"
           "Puppetfile"
           "Berksfile"
           "Brewfile"
           "Fastfile"
           "Vagrantfile"
           "Guardfile"
           "Podfile")
    :hook
    ((ruby-base-mode-hook . eglot-ensure)))

;; Terraform

(use-package terraform-mode
    :if (package-installed-p 'terraform-mode)
    :mode ("\\.tf" "\\.tfvars" "\\.tfbackend")
    :hook
    ((terraform-mode-hook . eglot-ensure)))

;; YAML

(use-package yaml-ts-mode
    :mode ("\\.ya?ml\\'")
    :config
    (add-to-list 'eglot-server-programs
                 '((yaml-ts-mode yaml-mode)
                   .
                   ("npx" "yaml-language-server" "--stdio")))
    :hook
    ((yaml-ts-mode-hook . eglot-ensure)))

;;; init-prog.el ends here
