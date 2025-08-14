;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; $DOOMDIR/packages.el

(package! exec-path-from-shell)

(when (modulep! :tools lsp +eglot)
  (package! eglot-booster
    :recipe (:type git
             :host github
             :repo "jdtsmith/eglot-booster")))

(package! devdocs)

(when (modulep! :tools lsp -eglot)
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))

(package! claude-code
  :recipe (:type git
           :host github
           :repo "stevemolitor/claude-code.el"
           :branch "main"
           :depth 1
           :files ("*.el" (:exclude "images/*"))))
(package! monet
  :recipe (:type git
           :host github
           :repo "stevemolitor/monet"))
