;;; config-javascript.el --- Setup JS/TS devel -*- lexical-binding: t -*-;;
;;; Commentary:
;;
;;; Code:

(use-package js2-mode
  :config
  (setq js-chain-indent t
        js2-basic-offset 2
        js2-skip-preprocessor-directives t
        js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t
        js2-strict-missing-semi-warning t
        js2-highlight-level 3
        js2-idle-timer-delay 0.15))

(use-package typescript-mode
  :config
  (setq typescript-auto-indent-flag t
        typescript-autoconvert-to-template-flag t
        typescript-indent-level 2
        typescript-indent-list-items t
        typescript-ident-switch-clauses 2))

(use-package js2-refactor
  :hook
  ((js2-mode . js2-refactor-mode)
   (typescript-mode . js2-refactor-mode)))

(use-package npm-mode
  :hook
  ((js-mode . npm-mode)
   (typescript-mode . npm-mode)))

(use-package rjsx-mode
  :interpreter "node"
  :init
  (add-to-list 'auto-mode-alist '("\\.[mc]?js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.pac\\'" . rjsx-mode))
  :config
  (setq js-switch-indent-offset js2-basic-offset))

(use-package web-mode)

(provide 'config-javascript)
;;; config-javascript.el ends here
