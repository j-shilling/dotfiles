;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq modus-themes-mode-line '(borderless)
      modus-themes-diffs 'desaturated
      modus-themes-deuteranopia t
      modus-themes-fringes nil

      modus-themes-operandi-color-overrides
      '((fg-window-divider-inner . "#ffffff")
        (fg-window-divider-outer . "#ffffff"))
      modus-themes-vivendi-color-overrides
      '((fg-window-divider-inner . "#000000")
        (fg-window-divider-outer . "#000000")))
(setq doom-theme 'modus-vivendi)

(use-package! vertico-multiform
  :hook (vertico . vertico-multiform-mode)
  :init
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          (info-menu buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-lsp-symbols buffer)
          (consult-xref buffer)
          (embark-keybinding buffer)
          (consult-location buffer))
        vertico-multiform-commands
        '((telega-chat-with buffer)
          (magit:--author flat)
          (Info-goto-node buffer)
          (info-lookup-symbol buffer)
          (Info-follow-reference buffer)
          (consult-yank-pop buffer))))

(use-package! epa
  :config
  (setq epa-pinentry-mode 'loopback
        epa-file-select-keys nil
        epa-file-encrypt-to "0FCC8E6A96FF109F"
        epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save t))

(use-package! grep
  :config
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package! smartparens
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-n" . sp-up-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("C-)"   . sp-forward-slurp-sexp)
        ("C-("   . sp-backward-slurp-sexp)
        ("C-}"   . sp-forward-barf-sexp)
        ("C-{"   . sp-backward-barf-sexp)
        ("M-r"   . sp-raise-sexp)
        ("M-<up>" . sp-splice-sexp-killing-backward)
        ("M-<down>" . sp-splice-sexp-killing-forward)
        ("M-s"   . sp-splice-sexp)
        ("M-S"   . sp-split-sexp)
        ("M-J"   . sp-join-sexp))
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (eval-expression-minibuffer-setup . smartparens-strict-mode)
   (ielm-mode . smartparens-strict-mode)
   (lisp-mode . smartparens-strict-mode)
   (scheme-mode . smartparens-strict-mode)
   (geiser-repl-mode . smartparens-strict-mode)
   (clojure-mode . smartparens-strict-mode)
   (clojurec-mode . smartparens-strict-mode)
   (clojurescope-mode . smartparens-strict-mode)
   (cider-repl-mode . smartparens-strict-mode)))

(use-package! ocamlformat
  :custom
  (ocamlformat-enable 'disable))

(use-package! utop
  :hook
  (utop . (lambda ()
            (setq-local company-idle-delay nil))))
