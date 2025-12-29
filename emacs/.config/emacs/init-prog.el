;;; init-prog.el --- Configuration for programming -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

;; Prog Mode

(eval-when-compile
  (require 'rx))

(use-package display-line-numbers
  :hook
  ((prog-mode-hook . display-line-numbers-mode)))

(use-package elec-pair
  :hook
  ((prog-mode-hook . electric-pair-local-mode)))

(use-package whitespace
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  ((prog-mode-hook . whitespace-mode)))

(use-package prog-mode
  :hook
  ((prog-mode-hook . prettify-symbols-mode)))

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
  ((prog-mode-hook . apheleia-mode)))

(use-package project
  :custom
  (project-list-buffers #'project-list-buffers-ibuffer)
  (project-list-file (init-lib-state-file "project"))
  (project-switch-commands `((project-find-file "Find file" "f")
                             (project-find-regexp "Find regexp" "g")
                             (project-find-dir "Find directory" "d")
                             (project-dired "Dired" "D")
                             (project-eshell "Eshell" "e")
                             ,(if (package-installed-p 'magit)
                                  '(magit-project-status "Magit" "v")
                                '(project-vc-dir "VC-Dir" "v"))
                             (project-any-command "Other" "o"))))

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
                `(
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
                          :schemaStore (:enable t))
                  :terraform ( :path ,(executable-find "terraform")
                               :validation (:enableEnhancedValidation nil)
                               :experimentalFeatures (:prefillRequiredFields t))))
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

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . ielm)))

(use-package lisp-mode
  :custom
  (lisp-indent-function 'lisp-indent-function))

;;; CSS

(use-package css-mode
  :mode (("\\.css\\'" . css-ts-mode)))

;;; TypetScript / JavaScript

(use-package nvm
  :if (package-installed-p 'nvm))

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
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook
  ((typescript-ts-base-mode-hook . eglot-ensure)))

;; Web

(use-package web-mode
  :if (package-installed-p 'web-mode))

;; Ruby

(use-package rbenv
  :if (package-installed-p 'rbenv)
  :hook ((after-init-hook . global-rbenv-mode))
  :config
  (setopt rbenv-executable (executable-find "rbenv")))

(use-package inf-ruby
  :if (package-installed-p 'inf-ruby)
  :hook ((ruby-base-mode-hook . inf-ruby-minor-mode)))

(use-package robe
  :if (package-installed-p 'robe)
  :hook ((ruby-base-mode-hook . robe-mode)))

(use-package rspec-mode
  :if (package-installed-p 'rspec-mode)
  :hook ((ruby-base-mode-hook . rspec-mode)))

(use-package rvm
  :if (package-installed-p 'rvm)
  :hook ((ruby-base-mode-hook . rvm-activate-corresponding-ruby)))

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
  :config
  (add-to-list 'auto-insert-alist
               (cons (cons (rx "factories" (zero-or-more anychar) ".rb" line-end)
                           "FactorBot file")
                     (lambda () (tempel-insert 'factorybot)))))

(use-package rbs-mode
  :mode ("\\.rbs\\'")
  :config
  (add-to-list 'eglot-server-programs
               '(rbs-mode
                 "steep" "langserver")))

;; Terraform

(use-package terraform-mode
  :if (package-installed-p 'terraform-mode)
  :mode ("\\.tf" "\\.tfvars" "\\.tfbackend"))

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
