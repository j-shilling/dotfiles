;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! exec-path-from-shell)
(package! pinentry)
(package! eglot-booster
  :recipe (:type git
           :host github
           :repo "jdtsmith/eglot-booster"))
(package! devdocs)
