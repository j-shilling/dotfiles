;;; init.el -*- lexical-binding: t; -*-

(setenv "LSP_USE_PLISTS" "true")

(doom! :checkers
       (syntax +flymake +icons)
       (spell +everywhere +aspell)
       grammar

       :completion
       (vertico +icons)
       (corfu +icons +orderless +dabbrev)

       :config
       (default +bindings +smartparens +gnupg)

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
       (cc +lsp)
       common-lisp
       emacs-lisp
       (scheme +guile +chez)
       (clojure +lsp +tree-sitter)
       (haskell +lsp)
       (javascript +lsp +tree-sitter)
       (latex +fold +lsp)
       (markdown)
       (org +pandoc +present +pretty +roam2)
       (php +lsp +tree-sitter)
       (python +lsp +pyenv +poetry +tree-sitter +pyright)
       (sh +lsp)
       (web +lsp)
       (yaml +lsp)

       :term
       eshell
       vterm

       :tools
       biblio
       llm
       tree-sitter
       direnv
       (docker +lsp +tree-sitter)
       (eval +overlay)
       (lookup +dictionary +docsets)
       (lsp)
       (magit +forge)
       make
       (pass +auth)
       pdf
       (terraform +lsp +tree-sitter)

       :ui
       doom
       hl-todo
       indent-guides
       ligatures
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       vc-gutter
       window-select
       zen)
