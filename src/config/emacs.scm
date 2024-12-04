(define-module (config emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde home services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
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
         (elisp-packages (list emacs-use-pacakge
                               emacs-diminish))))))

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
   (feature-emacs-vlf-config)
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
