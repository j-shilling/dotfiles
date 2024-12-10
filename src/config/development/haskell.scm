(define-module (config development haskell)
  #:use-module (rde features)
  #:use-module (rde home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-xyz)
  #:export (haskell-features))


;;; add source ~/.ghcup/env to profile
(define (feature-haskell)
  (define f-name 'haskell)

  (define (get-home-services config)
    (list
     (simple-service
      'add-haskell-tree-sitter
      home-profile-service-type
      (list
       tree-sitter-haskell))
     (simple-service
      'emacs-extensions
      home-emacs-service-type
      (home-emacs-extension
       (elisp-packages
        (list
         emacs-haskell-mode
         emacs-haskell-snippets))
       (init-el
        `((eval-when-compile (require 'use-package))
          (use-package haskell-mode
                       :mode "\\.[l]?hs\\'"
                       :interpreter "ghci"
                       :hook (haskell-mode . eglot-ensure))
          (use-package haskell-cabal
                       :mode ("\\.cabal\\'" . haskell-cabal-mode))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter  get-home-services)))

(define (haskell-features)
  (list
   (feature-haskell)))

(haskell-features)
