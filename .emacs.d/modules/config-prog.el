;;; Config-prog.el --- Setup general programming stuff -*- lexical-binding: t -*-;;
;;; Commentary:
;;
;;; Code:

;;;
;;; Prog Mode Config
;;;

(use-package display-line-numbers
  :straight nil
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package whitespace
  :straight nil
  :hook
  (prog-mode . whitespace-mode))

;;;
;;; Project Management
;;;

(use-package project
  :straight nil
  :custom
  (project-list-file (init-state-path "projects")))

(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;;;
;;; Error Handling
;;;

(use-package flymake
  :straight nil
  :hook
  (prog-mode . flymake-mode-on)
  :bind
  (:map flymake-mode-map
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e l" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-fringe-indicator-position nil))

;;;
;;; Git
;;;

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x p v" . magit-project-status)
   ([remap project-vc-dir] . magit-project-status))
  :init
  (with-eval-after-load 'project
    (defvar project-switch-commands)
    (declare-function assq-delete-all (key alist))
    (customize-set-value
     'project-switch-commands
     (cons '(magit-project-status "Magit")
           (assq-delete-all 'project-vc-dir project-switch-commands)))))
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

;;;
;;; LSP Support
;;;

;;; Taken from crafted emacs
(defconst config-prog--eglot-exclude
  '(clojure-mode
    lisp-mode
    scheme-mode
    tuareg-mode))
(defun config-prog--add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to the hook for each mode int MODE-LIST."
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (if (listp mode)
          (config-prog--add-eglot-hooks mode)
        (when (and (fboundp mode)
                   (not (memq mode config-prog--eglot-exclude)))
          (let ((hook (intern (concat (symbol-name mode) "-hook"))))
            (declare-function init-log (message &rest args))
            (init-log "Adding eglot to " (symbol-name hook))
            (add-hook hook #'eglot-ensure)))))))
(use-package eglot
  :demand
  :config
  (config-prog--add-eglot-hooks eglot-server-programs)
  :custom
  (eglot-autoshutdown t))
(use-package consult-eglot)

;;;
;;; Check for Tree Sitter
;;;

(when (treesit-available-p)
  (let ((ts-mod-dir (expand-file-name "treesit-modules" user-emacs-directory)))
    (when (file-directory-p ts-mod-dir)
      (add-to-list 'treesit-extra-load-path ts-mod-dir))))

(provide 'config-prog)
;;; config-prog.el ends here
