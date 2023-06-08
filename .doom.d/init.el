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
 (dired +icons)
 (ibuffer +icons)
 undo

 :lang
 (cc +lsp +tree-sitter)
 (clojure +tree-sitter)
 common-lisp
 (scala +lsp +tree-sitter)
 (csharp +lsp +tree-sitter)
 emacs-lisp
 (java +lsp +tree-sitter)
 (javascript +lsp +tree-sitter)
 (json +lsp +tree-sitter)
 (markdown +grip)
 (ocaml +tree-sitter)
 (org +pandoc +pretty +roam2 +present)
 (python +lsp +pyenv +tree-sitter)
 (rest +jq)
 (sh +lsp +tree-sitter)
 (web +lsp +tree-sitter)
 (yaml +lsp +tree-sitter)
 (graphql +lsp)

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
 editorconfig
 (eval +overlay)
 (lookup +dictionary +docsets)
 (lsp)
 (magit +forge)
 make
 (pass +auth)
 pdf
 terraform
 tree-sitter

 :email
 (mu4e +gmail +org)

 :ui
 hl-todo
 indent-guides
 (ligatures +fira)
 modeline
 ophints
 nav-flash
 unicode
 window-select
 zen)
