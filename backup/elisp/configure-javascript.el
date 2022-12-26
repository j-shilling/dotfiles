;;; configure-javascript.el -- JavaScript/TypeScript -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(add-hook 'js-mode-hook 'eglot)
(add-hook 'js-mode-hook
          (lambda (&rest _)
            (setq tab-width 2)))

;;; configure-javascript.el ends here
