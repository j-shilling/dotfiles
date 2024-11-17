(define-module (config home services)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services emacs)
  #:export (emacs-services))

(define emacs-packages
  (list
   emacs-ace-window
   emacs-apheleia
   emacs-consult
   emacs-corfu
   emacs-diff-hl
   emacs-diminish
   emacs-envrc
   emacs-eshell-syntax-highlighting
   emacs-eshell-vterm
   emacs-gcmh
   emacs-haskell-mode
   emacs-helpful
   emacs-ibuffer-vc
   emacs-ligature
   emacs-magit
   emacs-marginalia
   emacs-modus-themes
   emacs-nerd-icons
   emacs-nerd-icons-completion
   emacs-nerd-icons-corfu
   emacs-nerd-icons-dired
   emacs-nerd-icons-ibuffer
   emacs-nix-mode
   emacs-nix-ts-mode
   emacs-no-littering
   emacs-orderless
   emacs-poetry
   emacs-smartparens
   emacs-terraform-doc
   emacs-terraform-mode
   emacs-tsc
   emacs-vertico
   emacs-vlf
   emacs-vterm
   emacs-yasnippet
   emacs-guix))

(define emacs-services
  (list
   (service home-emacs-service-type
	    (home-emacs-configuration
	     (package emacs-next-tree-sitter)
	     (elisp-packages emacs-packages)
	     (init-el (slurp-file-like (local-file "../../../files/init.el")))
	     (early-init-el (slurp-file-like (local-file "../../../files/early-init.el")))))))
