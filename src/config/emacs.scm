(define-module (config emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde home services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (config packages emacs-xyz)
  #:export (emacs-features))

(define %extra-init-el
  '((use-pacakge emacs
                 :custom
                 (use-short-ansers t)
                 (ring-bell-function #'ignore))))

(define %extra-early-init-el '())
(define %additional-elisp-packages '())

(define (feature-emacs-base-config)
  (define emacs-f-name 'base-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el
          `((eval-when-compile
             (require 'use-package))
            (use-package emacs
                         :custom
                         (user-full-name ,(get-value 'full-name config))
                         (user-mail-address ,(get-value 'email config))

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
                         (dolist (fn '(set-default-coding-systems
                                       prefer-coding-system
                                       set-terminal-coding-system
                                       set-keyboard-coding-system
                                       set-buffer-file-coding-system
                                       set-selection-coding-system))
                                 (apply fn (list 'utf-8-unix)))
                         (set-language-environment "English")

                         (setq-default indent-tabs-mode nil)
                         (setq-default tab-width 4))))
         (elisp-packages (list emacs-diminish
                               emacs-codeium))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (feature-emacs-performance-config)
  (define emacs-f-name 'performance-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el
          `((eval-when-compile
             (require 'use-package))
            (use-package emacs
                         :custom
                         (large-file-warning-threshold nil))
            (use-package vlf
                         :init
                         (require 'vlf-setup))
            (use-package gcmh
                         :diminish gcmh-mode
                         :hook (after-init . gcmh-mode))
            (use-package so-long
                         :diminish global-so-long-mode
                         :hook (after-init . global-so-long-mode))))
         (elisp-packages (list emacs-vlf
                               emacs-so-long
                               emacs-gcmh))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define* (feature-emacs-appearance-config #:key
          (wayland? #f))
  (define emacs-f-name 'appearance-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el
          `((eval-when-compile
             (require 'use-package))

            ,@(if wayland?
                  (list
                   '(use-package pixel-scroll
                                 :diminish pixel-scroll-precision-mode
                                 :hook (after-init . pixel-scroll-precision-mode)))
                  (list))

            (use-package display-line-numbers
                         :diminish display-line-numbers-mode
                         :hook
                         (prog-mode . (lambda () display-line-numbers-mode +1))
                         (text-mode . (lambda () display-line-numbers-mode -1)))

            (use-package whitespace
                         :diminish whitespace-mode
                         :custom (whitepsace-action '(cleanup auto-cleanup))
                         :hook
                         (prog-mode . (lambda () (whitespace-mode +1)))
                         (text-mode . (lambda () (whitespace-mode -1))))

            (use-package ligature
                         :functions ligature-set-ligatures
                         :hook
                         (init-hook . global-ligature-mode)
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
                                                   ("!" (rx (+ (or "=" "!" "\\." ":" "~"))))
                                                   ;; ?? ??? ?:  ?=  ?.
                                                   ("?" (rx (or ":" "=" "\\." (+ "?"))))
                                                   ;; %% %%%
                                                   ("%" (rx (+ "%")))
                                                   ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                                                   ;; |->>-||-<<-| |- |== ||=||
                                                   ;; |==>>==<<==<=>==//==/=!==:===>
                                                   ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\\]"
                                                                   "-" "=" ))))
                                                   ;; \\ \\\ \/
                                                   ("\\" (rx (or "/" (+ "\\"))))
                                                   ;; ++ +++ ++++ +>
                                                   ("+" (rx (or ">" (+ "+"))))
                                                   ;; :: ::: :::: :> :< := :// ::=
                                                   (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                                                   ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                                                   ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\\*" ":" "!"
                                                                   "="))))
                                                   ;; .. ... .... .= .- .? ..= ..<
                                                   ("\\." (rx (or "=" "-" "\\?" "\\.=" "\\.<" (+ "\\."))))
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
                                                   ("<" (rx (+ (or "\\+" "\\*" "\\$" "<" ">" ":" "~"  "!"
                                                                   "-"  "/" "|" "="))))
                                                   ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                                                   ;; >> >>> >>>>
                                                   (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                                                   ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                                                   ("#" (rx (or ":" "=" "!" "(" "\\?" "\\[" "{" "_(" "_"
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
                                                   "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))))

         (elisp-packages (list emacs-ligature))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define* (feature-emacs-editing-config)
  (define emacs-f-name 'editing-config)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (simple-service
        'emacs-extensions
        home-emacs-service-type
        (home-emacs-extension
         (init-el
          `((eval-when-compile
             (require 'use-package))

            (use-package delsel
                         :diminish delete-selection-mode
                         :hook (after-init . delete-selection-mode))

            (use-package autorevert
                         :diminish global-auto-revert-mode
                         :custom (global-auto-revert-non-file-buffers t)
                         :hook (after-init . global-auto-revert-mode))

            (use-package flyspell
                         :diminish (flyspell-prog-mode flyspell-mode)
                         :hook
                         (prog-mode . flyspell-prog-mode)
                         (text-mode . flyspell-mode))

            (use-package subword
                         :diminish subword-mode
                         :hook (prog-mode . subword-mode))

            (use-package wgrep
                         :hook (grep-setup . wgrep-setup))

            (use-package multiple-cursors
                         :commands mc/sort-regions
                         :bind
                         ("C-S-c C-S-c" . mc/edit-lines)
                         ("C->" . mc/mark-next-like-this)
                         ("C-<" . mc/mark-previous-like-this)
                         ("C-c C-<" . mc/mark-all-like-this))))

         (elisp-packages (list emacs-wgrep
                               emacs-multiple-cursors))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define* (emacs-features
          #:key
          (wayland? #f))
  (list
   (let ((emacs-pkg (if wayland?
                        emacs-pgtk
                        emacs)))
     (feature-emacs
      #:emacs emacs-pkg
      #:emacs-server-mode? #t
      #:default-terminal? #f
      #:default-application-launcher? #f))
   (feature-emacs-base-config)
   (feature-emacs-performance-config)
   (feature-emacs-appearance-config #:wayland? wayland?)
   (feature-emacs-editing-config)
   (feature-emacs-appearance)
   (feature-emacs-modus-themes
    #:dark? #t)
   (feature-emacs-which-key)
   (feature-emacs-all-the-icons)
   (feature-emacs-tramp)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-completion)
   (feature-emacs-vertico)
   (feature-emacs-corfu)
   (feature-emacs-project)
   (feature-emacs-ace-window)
   (feature-emacs-smartparens)
   (feature-emacs-eglot)
   (feature-emacs-dape)
   (feature-emacs-flymake)
   (feature-emacs-git)
   (feature-emacs-guix)
   (feature-emacs-xref)
   (feature-emacs-pdf-tools)
   (feature-emacs-help)
   (feature-emacs-info)
   (feature-emacs-devdocs)
   (feature-emacs-org)
   (feature-emacs-org-roam
    #:org-roam-directory "~/org/roam"
    #:org-roam-todo? #t)
   (feature-emacs-citation)
   (feature-emacs-spelling)))
