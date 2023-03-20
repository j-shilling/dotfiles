;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom!
 :app
 everywhere
 irc

 :checkers
 syntax
 (spell +flyspell +everywhere)
 grammar

 :completion
 (vertico +icons)
 (company +childframe)

 :config
 (default +bindings +smartparens)

 :editor
 file-templates
 fold
 format
 multiple-cursors
 snippets
 word-wrap

 :emacs
 dired
 ibuffer
 undo

 :lang
 (cc +lsp)
 clojure
 common-lisp
 (csharp +lsp)
 emacs-lisp
 (java +lsp)
 (javascript +lsp)
 (json +lsp)
 (markdown +grip)
 ocaml
 (org +pandoc +pretty +roam2)
 (python +lsp +pyenv)
 rest
; (scheme +guile)
 (sh +lsp)
 (web +lsp)
 (yaml +lsp)

 :os
 tty

 :term
 eshell
 vterm

 :tools
 ansible
 (debugger +lsp)
 direnv
 (docker +lsp)
 (eval +overlay)
 (lookup +dictionary +docsets)
 (lsp)
 (magit)
 make
 (pass +auth)
 pdf
 terraform

 :ui
 (emoji +ascii +github +unicode)
 hl-todo
 indent-guides
 (ligatures +fira)
 modeline
 ophints
 window-select)
