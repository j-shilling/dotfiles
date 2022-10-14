;;; configure-defaults.el -- Defaults -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(add-hook 'prog-mode-hook #'flymake-mode-on)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;; configure-defaults.el ends here
