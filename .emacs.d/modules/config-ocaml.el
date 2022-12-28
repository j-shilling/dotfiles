;;; config-ocaml --- Setup for OCAML -*- lexical-binding: t -*-

(defun config-ocaml-setup-h ()
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

  (when (require 'merlin nil t)
    (require 'merlin-imenu)

    (setq merlin-command 'opam
          merlin-completion-with-doc t)

    (merlin-mode)
    (merlin-use-merlin-imenu))

  (require 'ocp-indent nil t)
  (require 'ocamlformat nil t)

  (when (require 'dune nil t)
    (add-to-list 'auto-mode-alist
                 '("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\)?\\'" . dune-mode))
    (add-hook 'dune-mode-hook
              (lambda ()
                (require 'dune-flymake)
                (flymake-mode-on))))

  (when (require 'utop nil t)
    (utop-minor-mode)))

(use-package tuareg
  :hook
  (tuareg-mode . config-ocaml-setup-h))

(provide 'config-ocaml)
;;; config-ocaml.el ends here
