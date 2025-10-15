;;; init-prog.el --- Configuration for programming -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

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

;;; init-prog.el ends here
