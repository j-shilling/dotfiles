;;; config-lisp.el --- Setup Lisp Modes -*- lexical-binding: t -*-

(defconst init-lisp-modes
  '(emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-mode
    scheme-mode
    geiser-repl-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    cider-repl-mode))

(defun init-list-mode->hook (mode)
  "Derive a hook variable name from `MODE'."
  (declare (pure t) (side-effect-free t))
  (let ((mode-name (cond
                    ((symbolp mode) (symbol-name mode))
                    ((stringp mode) mode)
                    (t (signal 'wrong-type-argument '(stringp mode))))))
    (intern (format "%s-hook" mode-name))))

(defun init-lisp-hooks (&optional lisp-modes)
  "Return symbols refering to hook vars derrived from mode names.

If `LISP-MODES' is given, this list will be converted. Otherwise,
`init-lisp-modes' will be used."
  (declare (pure t) (side-effect-free t))
  (mapcar #'init-list-mode->hook
          (or lisp-modes
              init-lisp-modes)))

(use-package paredit
  :init
  (dolist (hook (init-lisp-hooks))
    (add-hook hook #'enable-paredit-mode)))

(use-package paren
  :straight nil
  :init
  (dolist (hook (init-lisp-hooks))
    (add-hook hook #'show-paren-mode)))

(use-package clojure-mode)
(use-package cider
  :hook
  ((cider-mode . eldoc-mode))
  :init
  (setq org-babel-clojure-backend 'cider)
  :config
  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically t
        cider-overlays-use-font-lock t
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (init-cache-path "cider-repl-history-file")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-destroy
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil))

(provide 'config-lisp)
;;; config-lisp.el ends here
