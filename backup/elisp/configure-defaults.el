;;; configure-defaults.el -- Defaults -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(add-hook 'prog-mode-hook #'flymake-mode-on)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)


(autoload 'pinentry-start "pinentry")
(add-hook 'after-init-hook #'pinentry-start)

(autoload 'delete-selection-mode "delsel")
(add-hook 'after-init-hook #'delete-selection-mode)

;;; configure-defaults.el ends here
