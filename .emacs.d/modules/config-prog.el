;;; config-prog.el --- Setup general programming stuff -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'flymake-mode-on)

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x p v" . magit-project-status)
   ([remap project-vc-dir] . magit-project-status))
  :init
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m") t))
(use-package magit-todos)
(use-package git-timemachine)
(use-package diff-hl
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (init-first-file . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (diff-hl-mode . diff-hl-margin-mode)))

(use-package project
  :straight nil
  :init
  (setq project-list-file
        (init-state-path "projects")
        project-switch-commands
        (assq-delete-all 'project-vc-dir project-switch-commands)))
(use-package consult-eglot)
(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))
(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(when (treesit-available-p)
  (let ((ts-mod-dir (expand-file-name "treesit-modules" user-emacs-directory)))
    (when (file-directory-p ts-mod-dir)
      (add-to-list 'treesit-extra-load-path ts-mod-dir))))

(provide 'config-prog)
;;; config-prog.el ends here
