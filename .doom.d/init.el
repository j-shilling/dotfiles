;;; init.el -*- lexical-binding: t; -*-

(doom!
 :app
 everywhere
 irc
 (rss +org)

 :checkers
 (syntax +flymake)
 (spell +everywhere)
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
 (clojure +tree-sitter)
 sml
 common-lisp
 (csharp +lsp +tree-sitter)
 (haskell +lsp +tree-sitter)
 emacs-lisp
 (javascript +lsp +tree-sitter)
 (json +lsp +tree-sitter)
 markdown
 (latex +lsp +fold)
 (nix +lsp +tree-sitter)
 (ocaml +tree-sitter)
 (org +pandoc +pretty +roam2 +present)
 (plantuml)
 (python +lsp +tree-sitter +pyenv +pyright)
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
 biblio
 debugger
 direnv
 (docker +lsp)
 editorconfig
 (eval +overlay)
 (lookup +dictionary +docsets)
 (lsp +eglot)
 (magit +forge)
 make
 (pass +auth)
 pdf
 terraform
 tree-sitter

 :email
 (mu4e +gmail +org)

 :ui
 deft
 hl-todo
 indent-guides
 (ligatures +fira)
 modeline
 ophints
 nav-flash
 (popup +defaults)
 unicode
 window-select
 (vc-gutter +pretty)
 zen)
