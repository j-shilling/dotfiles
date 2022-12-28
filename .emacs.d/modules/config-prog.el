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
   (dired-mode . diff-hl-dired-mode)))
(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position nil))

(use-package project
  :straight nil
  :init
  (setq project-list-file
        (init-state-path "projects")
        project-switch-commands
        (assq-delete-all 'project-vc-dir project-switch-commands)))

;;; Taken from crafted emacs
(defconst config-prog--eglot-exclude
  '(clojure-mode
    lisp-mode
    scheme-mode
    tuareg-mode))
(defun config-prog--add-eglot-hooks (mode-list)
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (if (listp mode)
          (config-prog--add-eglot-hooks mode)
        (when (and (fboundp mode)
                   (not (memq mode config-prog--eglot-exclude)))
          (let ((hook (intern (concat (symbol-name mode) "-hook"))))
            (init-log "Adding eglot to " (symbol-name hook))
            (add-hook hook #'eglot-ensure)))))))
(use-package consult-eglot
  :init
  (config-prog--add-eglot-hooks eglot-server-programs)
  :custom
  (eglot-autoshutdown t))

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
