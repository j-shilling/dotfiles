;;; init.el --- Initialization -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;;;
;;; System Information
;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-WSL     (and IS-LINUX
                          (string-match-p "Microsoft"
                                          (shell-command-to-string "uname -a"))))

;;;
;;; Setup package loading
;;;

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (require 'diminish)
  ;; Currently experimenting with having Nix/Guix install emacs
  ;; packages, so I want to disable the ensure function.
  (setq use-package-ensure-function
        (lambda (&rest _) t))
  ;; I get confused about the actual names of hook vars when they
  ;; aren't typed out.
  (setq use-package-hook-name-suffix nil)
  (setq use-package-always-defer t))

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; Keep files out of `user-emacs-directory'
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :defines (no-littering-etc-directory no-littering-var-directory)
  :init
  (setq no-littering-var-directory
        (expand-file-name "emacs"
                          (or (when IS-WINDOWS
                                (getenv "LOCALAPPDATA"))
                              (getenv "XDG_CACHE_HOME")
                              "~/.cache"))
        no-littering-etc-directory
        (expand-file-name "emacs"
                          (or (when IS-WINDOWS
                                (getenv "APPDATA"))
                              (getenv "XDG_CONFIG_HOME")
                              "~/.config/emacs/etc")))
  (require 'no-littering))

;;;
;;; Performance Stuff
;;;

(setq auto-mode-case-fold nil)

(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq jit-lock-defer-time 0)

(use-package gcmh
  :diminish gcmh-mode
  :hook
  (after-init-hook . gcmh-mode)
  :custom
  (gcmh-verbose init-file-debug))

(use-package vlf
  :init
  ;; This sets-up all the autoloads and hooks
  (require 'vlf-setup))

;;;
;;; General Setup
;;;

(use-package emacs
  :custom
  (user-full-name    "Jake Shilling")
  (user-mail-address "shilling.jake@gmail.com")

  (use-short-answers  t)
  (ring-bell-function #'ignore)

  (auto-save-default  t)
  (auto-save-timeout  20)
  (auto-save-interval 200)

  (make-backup-files    t)
  (vc-make-backup-files nil)
  (backup-by-copying    t)
  (version-control      t)
  (kept-old-versions    6)
  (kept-new-versions    9)

  (delete-by-moving-to-trash nil)

  (kill-ring-max 120)
  (kill-do-not-save-duplicates t)

  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

  (vc-follow-symlinks t)
  ;; Don't warn because VLF will
  (large-file-warning-threshold nil)

  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)

  (require-final-newline t)

  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)

  (sentence-end-double-space nil)
  :init
  (setq native-comp-jit-compilation nil)
  (setq custom-file nil)
  (setq ring-bell-function #'ignore)

  (let ((encoding (if IS-WINDOWS
                      'utf-8-dos
                    'utf-8-unix)))
    (cl-loop for fn in '(set-default-coding-systems
                         prefer-coding-system
                         set-terminal-coding-system
                         set-keyboard-coding-system
                         set-buffer-file-coding-system
                         set-selection-coding-system)
             do (apply fn (list encoding))))
  (set-language-environment "English")

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (when IS-WSL
    ;; WSLg breaks copy-paste from Emacs into Windows
    ;; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
    (setq select-active-regions nil
          select-enable-clipboard 't
          select-enable-primary nil
          interprogram-cut-function #'gui-select-text)))

(use-package so-long
  :demand
  :diminish global-so-long-mode
  :hook
  (after-init-hook . global-so-long-mode))

;;;
;;; History
;;;

(use-package recentf
  :diminish recentf-mode
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 300)
  :hook
  (after-init-hook recentf-mode))

;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :diminish savehist-mode
  :custom
  (savehist-additional-variables '(kill-ring
                                   command-history
                                   set-variable-value-history
                                   query-replace-history
                                   read-expression-history
                                   minibuffer-history
                                   read-char-history
                                   face-name-history
                                   bookmark-history
                                   file-name-history))
  :hook
  (after-init-mode . savehist-mode))

(use-package saveplace
  :diminish save-place-mode
  :custom
  (save-place-forget-unreadable-files t)
  :hook
  (after-init-hook . save-place-mode))

;;;
;;; Appearance
;;;

(use-package fontset
  :init
  (setq use-default-font-for-symbols nil)
  :config
  (set-fontset-font t 'symbol "Noto Emoji" nil 'append)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  (set-fontset-font "fontset-default" nil
                    (font-spec :name "Noto Emoji")))

(use-package fontaine
  :after fontset
  :config
  (setq fontaine-current-preset t
        fontaine-presets
        '((t
           :default-family "Iosevka"
           :default-height 11
           :fixed-pitch-family "Iosevka"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Etoile"
           :variable-pitch-height 1.0
           :variable-pitch-weight regular)
          (regular)
          (large :default-weight semilight
                 :default-height ,(+ 11 40)
                 :bold-weight extrabold)))
  :hook
  (after-init-hook . 'fontaine-mode))

(use-package ligature
  :functions ligature-set-ligatures
  :hook
  (after-init-hook . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))

(use-package pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package display-line-numbers
  :diminish display-line-numbers-mode
  :hook
  (prog-mode-hook . (lambda () (display-line-numbers-mode +1)))
  (text-mode-hook . (lambda () (display-line-numbers-mode -1))))

(use-package whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  :hook
  (prog-mode-hook . (lambda () (whitespace-mode +1)))
  (text-mode-hook . (lambda () (whitespace-mode -1))))

;;;
;;; Completion
;;;

(use-package consult
  :bind ;; C-c bindings (mode-specific-map)
  (("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x r" . consult-recent-file)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-fd)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-ripgrep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))

  :hook (completion-list-mode-hook . consult-preview-at-point-mode)

  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package minibuffer
  :bind
  ("C-M-i" . completion-at-point))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (yas-minor-mode-hook . (lambda ()
                           (setq-local hippie-expand-try-functions-list
                                       (cons #'yas-hippie-try-expand hippie-expand-try-functions-list)))))

(use-package embark
  :autoload embark-prefix-help-command
  :bind
  (("C-." . embark-act)
   ("C-;". embark-dwim)
   ("C-h B" . embark-become))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;
;;; Editing
;;;

(use-package envrc
  :diminish (envrc-global-mode envrc-mode)
  :hook (after-init-hook . envrc-global-mode))

(use-package delsel
  :diminish delete-selection-mode
  :hook (after-init-hook . delete-selection-mode))

(use-package autorevert
  :diminish global-auto-revert-mode
  :custom
  (global-auto-revert-non-file-buffers t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package flyspell
  :diminish (flyspell-prog-mode flyspell-mode)
  :hook
  (prog-mode-hook . flyspell-prog-mode)
  (text-mode-hook . flyspell-mode))

(use-package multiple-cursors
  :commands mc/sort-regions
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package subword
  :diminish subword-mode
  :hook
  (prog-mode-hook . subword-mode))

(use-package wgrep
  :hook
  (grep-setup-hook . 'wgrep-setup))

;;;
;;; Tools
;;;

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-listing-switches "-lah -v --group-directories-first")
  :hook
  ((dired-mode-hook . dired-omit-mode)
   (dired-mode-hook . dired-hide-details-mode)))

(use-package diredfl
  :hook
  (after-init-hook . diredfl-global-mode))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package eshell
  :custom
  (eshell-modules-list '(eshell-alias
                         eshell-banner
                         eshell-cmpl
                         eshell-dirs
                         eshell-elecslash
                         eshell-extpipe
                         eshell-glob
                         eshell-hist
                         eshell-ls
                         eshell-prompt
                         eshell-script
                         eshell-smart
                         eshell-tramp
                         eshell-unix
                         eshell-xtra))
  (eshell-visual-commands '("pnpm"
                            "yarn"
                            "npx"
                            "flatpak"
                            "docker"
                            "docker-compose"
                            "devcontainer"))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  :hook
  (eshell-mode-hook . (lambda ()
                        (setenv "TERM" "xterm-256color"))))

(use-package eshell-syntax-highlighting
  :hook
  (eshell-mode-hook . eshell-syntax-highlighting-mode))

(use-package diff-hl
  :diminish (diff-hl-mode diff-hl-dir-mode)
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (prog-mode-hook . diff-hl-mode)
  (vc-dir-mode . diff-hl-dir-mode))

(use-package diff-hl-dired
  :diminish diff-hl-dired-mode
  :hook (dired-mode-hook . diff-hl-dired-mode))

;;;
;;; General Programming
;;;

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc--echo-area-prefer-doc-buffer-p t))

(use-package prog-mode
  :hook
  (prog-mode-hook . prettify-symbols-mode))

(use-package apheleia
  :diminish apheleia-mode
  :hook
  (after-init-hook . apheleia-global-mode))

(use-package smartparens
  :diminish smart-parens-mode
  :defines smartparens-mode-map
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-n" . sp-up-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("C-)"   . sp-forward-slurp-sexp)
        ("C-("   . sp-backward-slurp-sexp)
        ("C-}"   . sp-forward-barf-sexp)
        ("C-{"   . sp-backward-barf-sexp)
        ("M-r"   . sp-raise-sexp)
        ("M-<up>" . sp-splice-sexp-killing-backward)
        ("M-<down>" . sp-splice-sexp-killing-forward)
        ("M-s"   . sp-splice-sexp)
        ("M-S"   . sp-split-sexp)
        ("M-J"   . sp-join-sexp))
  :config
  ;; load default configuration to setup other pairs
  (require 'smartparens-config)
  (diminish smartparens-strict-mode)
  (diminish smartparens-mode)
  :hook
  ((emacs-lisp-mode-hook . smartparens-strict-mode)
   (eval-expression-minibuffer-setup-hook . smartparens-mode)
   (ielm-mode-hook . smartparens-strict-mode)
   (lisp-mode-hook . smartparens-strict-mode)
   (lisp-data-mode-hook . smartparens-strict-mode)
   (scheme-mode-hook . smartparens-strict-mode)
   (geiser-repl-mode-hook . smartparens-strict-mode)
   (clojure-mode-hook . smartparens-strict-mode)
   (clojurec-mode-hook . smartparens-strict-mode)
   (clojurescope-mode-hook . smartparens-strict-mode)
   (cider-repl-mode-hook . smartparens-strict-mode)
   (prog-mode-hook . smartparens-mode)
   (org-mode-hook . smartparens-mode)
   (markdown-mode-hook . smartparens-mode)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((js-json-mode json-mode json-ts-mode jsonc-mode)
                 .
                 ("npx" "vscode-json-languageserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((yaml-ts-mode yaml-mode)
                 .
                 ("npx" "yaml-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode)
                 .
                 ("npx" "-p" "dockerfile-language-server-nodejs" "docker-langserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 .
                 ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(astro-mode
                 .
                 ("npx" "-p" "@astrojs/language-server" "astro-ls" "--stdio"
                  :initializationOptions
                  (:typescript (:tsdk "./node_modules/typescript/lib")))))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-doc-buffer)))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

;;;
;;; Language Specific
;;;

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-hook . eglot-ensure))

(use-package typescript-ts-mode
  :mode (("\\.[m]?ts\\'" . typescript-ts-mode)
         ("\\.[m]?js\\'" . typescript-ts-mode)
         ("\\.[m]?tsx?\\'" . tsx-ts-mode)
         ("\\.[m]?jsx?\\'" . tsx-ts-mode))
  :interpreter "ts-node"
  :hook
  (typescript-ts-base-mode-hook . eglot-ensure))

(use-package python-ts-mode
  :mode "\\.py[iw]?\\'"
  :interpreter "python"
  :hook
  (python-ts-mode-hook . eglot-ensure))

(use-package pyvenv
  :hook
  ((python-ts-mode-hook . pyvenv-mode)
   (python-ts-mode-hook . pyvenv-tracking-mode)))

(use-package poetry
  :hook
  (python-ts-mode-hook . poetry-tracking-mode))

(use-package dockerfile-ts-mode
  :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
  :hook
  (dockerfile-ts-mode-hook . eglot-ensure))

(use-package haskell-mode
  :mode "\\.[l]?hs\\'"
  :interpreter "ghci"
  :init (require 'haskell-mode-autoloads)
  :hook
  (haskell-mode-hook . eglot-ensure))

(use-package haskell-cabal
  :mode ("\\.cabal\\'" . haskell-cabal-mode))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :config
  (add-to-list 'apheleia-mode-alist
               '(graphql-ts-mode . prettier-graphql)))

(use-package terraform-mode
  :mode ("\\.tf\\'")
  :hook
  (terraform-mode-hook . eglot-ensure))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package arei
  :init
  (setq geiser-mode-auto-p t)
  (setq ares-mode-auto-p nil))

;; TODO: Update this to use astro-ts-mode instead

(require 'web-mode)
(define-derived-mode astro-mode web-mode "astro")
(add-to-list 'auto-mode-alist '(".*\\.astro\\'" . astro-mode))


;;; init.el ends here
