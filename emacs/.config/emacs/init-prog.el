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

;;; init-prog.el ends here
