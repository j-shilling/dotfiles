(define-module (config home home-config)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home services dotfiles)
  #:use-module (config home services shell)
  #:use-module (config home services gpg))

(define emacs-packages
  (list
   emacs-compat
   emacs-ace-window
   emacs-apheleia
   emacs-consult
   emacs-corfu
   emacs-diff-hl
   emacs-diminish
   emacs-envrc
   emacs-eshell-syntax-highlighting
   ;; emacs-eshell-vterm
   emacs-gcmh
   emacs-haskell-mode
   emacs-helpful
   emacs-ibuffer-vc
   emacs-ligature
   emacs-magit
   emacs-marginalia
   emacs-modus-themes
   emacs-nerd-icons
   ;; emacs-nerd-icons-completion
   ;; emacs-nerd-icons-corfu
   ;; emacs-nerd-icons-dired
   ;; emacs-nerd-icons-ibuffer
   emacs-nix-mode
   ;; emacs-nix-ts-mode
   emacs-no-littering
   emacs-orderless
   ;; emacs-poetry
   emacs-smartparens
   ;; emacs-terraform-doc
   emacs-terraform-mode
   ;; emacs-tsc
   emacs-vertico
   emacs-vlf
   emacs-vterm
   emacs-yasnippet
   emacs-guix))

(home-environment
 (packages `(,emacs-next-tree-sitter ,@emacs-packages ,git))
 (services
  `(
    ,@shell-services
    ,@gpg-services
    ,@(list
       (service home-dotfiles-service-type
                (home-dotfiles-configuration
                 (directories '("../../files"))))))))
