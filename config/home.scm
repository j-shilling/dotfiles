(define-module (config home)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features version-control)
  #:use-module (rde features xdg)
  #:use-module (rde features docker)
  #:use-module (rde features documentation)
  #:use-module (rde features python)
  #:use-module (rde features ssh)
  #:use-module (rde features markup)
  #:use-module (rde features guile)
  #:use-module (rde features terminals)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (contrib features json)
  #:use-module (contrib features javascript)
  #:use-module (config packages node-xyz))

;;; Code:

(define minimal-rde-config
  (rde-config
   (features
    (list
     (feature-user-info
      #:user-name "jake"
      #:full-name "Jake Shilling"
      #:email "shilling.jake@gmail.com"
      #:emacs-advanced-user? #t)
     (feature-base-packages
      #:home-packages
      (list
       devcontainers-cli-0.72.0))
     ;; EMACS
     (feature-emacs
      #:native-comp? #t)
     (feature-emacs-appearance)
     (feature-emacs-modus-themes)
     (feature-emacs-which-key)
     (feature-emacs-all-the-icons)
     (feature-emacs-tramp)
     (feature-emacs-dired)
     (feature-emacs-eshell)
     (feature-emacs-completion)
     (feature-emacs-vertico)
     (feature-emacs-corfu)
     (feature-emacs-smartparens)
     (feature-emacs-eglot)
     (feature-emacs-flymake)
     (feature-emacs-git)
     (feature-emacs-geiser)
     (feature-emacs-guix)
     (feature-emacs-xref)
     (feature-vterm)
     (feature-ssh)
     (feature-xdg)
     (feature-bash)
     (feature-markdown)
     (feature-tex)
     (feature-python)
     (feature-direnv)
     (feature-compile)
     (feature-docker)
     (feature-manpages)
     (feature-javascript)
     (feature-gnupg
      #:gpg-primary-key "0FCC8E6A96FF109F"
      #:ssh-keys
      '(("E556265A9520AFE6C5BEC85C47B1ADB883CCBC91")))
     (feature-git
      #:sign-commits? #t)
     (feature-guile)))))

(rde-config-home-environment minimal-rde-config)
