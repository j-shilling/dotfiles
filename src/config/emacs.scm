(define-module (config emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (gnu packages emacs)
  #:export (emacs-features))

(define* (emacs-features
          #:key
          (wayland? #f))
  (list
   (let ((emacs-pkg (if wayland?
                        emacs-pgtk
                        emacs)))
     (feature-emacs
      #:emacs emacs-pkg
      #:emacs-server-mode? #t))
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
     (feature-emacs-smartparens)
     (feature-emacs-eglot)
     (feature-emacs-flymake)
     (feature-emacs-git)
     (feature-emacs-geiser)
     (feature-emacs-guix)
     (feature-emacs-xref)))
