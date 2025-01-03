(setq select-active-regions
      nil
      select-enable-clipboard
      't
      select-enable-primary
      nil
      interprogram-cut-function
      (function gui-select-text))
(eval-when-compile (require 'use-package))
(use-package
  emacs
  :custom
  (user-full-name "Jake Shilling")
  (user-mail-address "shilling.jake@gmail.com")
  (use-short-answers t)
  (ring-bell-function (function ignore))
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  (make-backup-files t)
  (vc-make-backup-files nil)
  (backup-by-copying t)
  (version-control t)
  (keep-old-versions 6)
  (keep-new-versions 9)
  (delete-by-moving-to-trash nil)
  (kill-ring-max 120)
  (kill-do-not-save-duplicate t)
  (vc-follow-symlinks t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
  (require-final-newline t)
  (text-mode-ispell-word-completion nil)
  (sentence-end-double-space nil)
  :init
  (dolist
    (fn '(set-default-coding-systems
           prefer-coding-system
           set-terminal-coding-system
           set-keyboard-coding-system
           set-buffer-file-coding-system
           set-selection-coding-system))
    (apply fn (list 'utf-8-unix)))
  (set-language-environment "English")
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4))
(eval-when-compile (require 'use-package))
(use-package emacs :custom (large-file-warning-threshold nil))
(use-package vlf :init (require 'vlf-setup))
(use-package gcmh :diminish gcmh-mode :hook (after-init . gcmh-mode))
(use-package
  so-long
  :diminish
  global-so-long-mode
  :hook
  (after-init . global-so-long-mode))
(eval-when-compile (require 'use-package))
(use-package
  pixel-scroll
  :diminish
  pixel-scroll-precision-mode
  :hook
  (after-init . pixel-scroll-precision-mode))
(use-package
  display-line-numbers
  :diminish
  display-line-numbers-mode
  :hook
  (prog-mode lambda () (display-line-numbers-mode 1))
  (text-mode lambda () (display-line-numbers-mode -1)))
(use-package
  whitespace
  :diminish
  whitespace-mode
  :custom
  (whitepsace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode lambda () (whitespace-mode 1))
  (text-mode lambda () (whitespace-mode -1)))
(use-package
  ligature
  :functions
  ligature-set-ligatures
  :hook
  (init-hook . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
    'prog-mode
    '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
      (";" (rx (+ ";")))
      ("&" (rx (+ "&")))
      ("!" (rx (+ (or "=" "!" "\\." ":" "~"))))
      ("?" (rx (or ":" "=" "\\." (+ "?"))))
      ("%" (rx (+ "%")))
      ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\\]" "-" "="))))
      ("\\" (rx (or "/" (+ "\\"))))
      ("+" (rx (or ">" (+ "+"))))
      (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
      ("/" (rx (+ (or ">" "<" "|" "/" "\\" "\\*" ":" "!" "="))))
      ("\\." (rx (or "=" "-" "\\?" "\\.=" "\\.<" (+ "\\."))))
      ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
      ("*" (rx (or ">" "/" ")" (+ "*"))))
      ("w" (rx (+ "w")))
      ("<" (rx (+ (or "\\+" "\\*" "\\$" "<" ">" ":" "~" "!" "-" "/" "|" "="))))
      (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
      ("#" (rx (or ":" "=" "!" "(" "\\?" "\\[" "{" "_(" "_" (+ "#"))))
      ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
      ("_" (rx (+ (or "_" "|"))))
      ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
      "Fl"
      "Tl"
      "fi"
      "fj"
      "fl"
      "ft"
      "{|"
      "[|"
      "]#"
      "(*"
      "}#"
      "$>"
      "^=")))
(eval-when-compile (require 'use-package))
(use-package
  delsel
  :diminish
  delete-selection-mode
  :hook
  (after-init . delete-selection-mode))
(use-package
  autorevert
  :diminish
  global-auto-revert-mode
  :custom
  (global-auto-revert-non-file-buffers t)
  :hook
  (after-init . global-auto-revert-mode))
(use-package
  flyspell
  :diminish
  (flyspell-prog-mode flyspell-mode)
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))
(use-package subword :diminish subword-mode :hook (prog-mode . subword-mode))
(use-package wgrep :hook (grep-setup . wgrep-setup))
(use-package
  multiple-cursors
  :commands
  mc/sort-regions
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))
(eval-when-compile (require 'use-package))
(use-package yaml-mode :mode "\\.y[a]?ml\\'")
(eval-when-compile (require 'use-package))
(use-package
  python-ts-mode
  :mode
  "\\.py[iw]?\\'"
  :interpreter
  "/gnu/store/hq9fsxcdk4lmsiki7gmsi049smsid9h0-python-next-3.12.2/bin/python3.12"
  :hook
  (python-ts-mode . eglot-ensure))
(use-package
  eglot
  :config
  (add-to-list
    'eglot-server-programs
    (cons '(python-mode python-ts-mode)
          (eglot-alternatives
            '(("basedpyright-langserver" "--stdio")
              "pylsp"
              "pyls"
              ("pyright-langserver" "--stdio")
              "jedi-language-server"
              "ruff-lsp")))))
(eval-when-compile (require 'use-package))
(use-package
  haskell-mode
  :mode
  "\\.[l]?hs\\'"
  :interpreter
  "ghci"
  :hook
  (haskell-mode . eglot-ensure))
(use-package haskell-cabal :mode ("\\.cabal\\'" . haskell-cabal-mode))
(setq geiser-mode-auto-p nil)
(setq ares-mode-auto-p t)
