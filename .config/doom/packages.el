;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! exec-path-from-shell)
(package! pinentry)

(when (modulep! :tools lsp +eglot)
  (package! eglot-booster
    :recipe (:type git
             :host github
             :repo "jdtsmith/eglot-booster")))

(package! devdocs)

(when (modulep! :tools lsp -eglot)
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))

(package! aidermacs)
