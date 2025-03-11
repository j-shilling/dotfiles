(use-modules (guix profiles)
             (gnu packages))

(specifications->manifest
 '("emacs-geiser"
   "emacs-geiser-guile"
   "emacs-apheleia"
   "emacs-pinentry"
   "emacs-guix"
   "emacs-pyvenv"
   "emacs-smartparens"
   "emacs-haskell-mode"
   "emacs-haskell-snippets"
   "emacs-diminish"
   "emacs-ligature"
   "emacs-wgrep"
   "emacs-multiple-cursors"
   "emacs-vlf"
   "emacs-so-long"
   "emacs-gcmh"
   "emacs-yaml-mode"
   "emacs-no-littering"
   "emacs-apheleia"
   "emacs-diff-hl"
   "emacs-diredfl"
   "emacs-consult-notmuch"
   "emacs-consult-dir"
   "emacs-consult-org-roam"
   "emacs-consult-yasnippet"
   "emacs-devdocs"
   "emacs-web-mode"
   "emacs-lsp-booster"
   "emacs-eglot-booster"
   "emacs-envrc"
   "emacs-fontaine"
   "emacs-eat"))
