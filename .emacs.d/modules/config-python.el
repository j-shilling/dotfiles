;;; config-python.el --- Setup Python development -*- lexical-binding: t -
;;
;;; Commentary:
;;
;;; Code:

(use-package python-mode)
(use-package pyvenv
  :hook
  ((python-mode pyvenv-mode)
   (pyvenv-mode pyvenv-tracking-mode)))
(use-package py-isort)

(provide 'config-python)
;;; config-python.el ends here
