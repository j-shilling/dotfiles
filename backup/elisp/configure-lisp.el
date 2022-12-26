(defconst init-lisp-modes
  '(emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-mode
    scheme-mode
    geiser-repl-mode
    clojure-mode
    clojurec-mode
    clojurescope-mode
    cider-repl-mode)
  "Installed modes which correspond to lisp-like languages.")

(defun init-list-mode->hook (mode)
  "Derrive a hook variable name from `MODE'."
  (declare (pure t) (side-effect-free t))
  (let ((mode-name (cond
                    ((symbolp mode) (symbol-name mode))
                    ((stringp mode) mode)
                    (t (signal 'wrong-type-argument '(stringp mode))))))
    (intern (format "%s-hook" mode-name))))

(defun init-lisp-hooks (&optional lisp-modes)
  "Return symbols refering to hook vars derrived from mode names.

          If `LISP-MODES' is given, this list will be converted.  Otherwise,
          `init-lisp-modes' will be used."
  (declare (pure t) (side-effect-free t))
  (mapcar #'init-list-mode->hook
          (or lisp-modes
              init-lisp-modes)))

(dolist (hook (init-lisp-hooks))
  (declare-function enable-paredit-mode "paredit" ())
  (add-hook hook #'enable-paredit-mode))

;; GUIX STUFF

(add-hook 'scheme-mode-hook 'guix-devel-mode)
