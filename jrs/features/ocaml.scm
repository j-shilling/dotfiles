(define-module (jrs features ocaml)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:export (feature-ocaml))

(define (feature-ocaml)

  (define (home-ocaml-services config)
    (list
     (rde-elisp-configuration-service
      'ocaml
      config
      '((with-eval-after-load
         'tuareg
         (setq tuareg-prettify-symbols-full t
               tuareg-opam-insinuate t)
         (tuareg-opam-update-env (tuareg-opam-current-compiler))
         (autoload 'merlin-mode "merlin" nil t)
         (add-hook 'tuareg-mode-hook 'merlin-mode)
         (autoload 'ocp-setup-indent "ocp-indent" nil t)
         (add-hook 'tuareg-mode-hook 'ocp-setup-indent)
         (add-hook 'tuareg-mode-hook 'utop-minor-mode))

        (with-eval-after-load
         'merlin
         (setq merlin-completion-with-doc t)
         (autoload 'merlin-use-merlin-imenu "merlin-imenu" nil t)
         (add-hook 'merlin-mode-hook 'merlin-use-merlin-imenu))

        (autoload 'ocamlformat-setup-indent "ocamlformat" nil t)
        (autoload 'ocamlformat-caml-mode-setup "ocamlformat" nil t)
        (autoload 'ocamlformat-before-save "ocamlformat" nil t)
        (autoload 'ocamlformat "ocamlformat" nil t)

        (autoload 'dune-mode "dune" nil t)
        (autoload 'dune-promote "dune" nil t)
        (autoload 'dune-runtest-and-promote "dune" nil t)
        (add-to-list 'auto-mode-alist
                     '("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\)?\\'" . dune-mode))
        (add-hook 'dune-mode-hook
                  (lambda ()
                    (require 'dune-flymake)
                    (flymake-mode-on)))

        (autoload 'utop "utop" nil t)
        (autoload 'utop-minor-mode "utop" nil t))
      #:elisp-packages
      (list emacs-tuareg))

     (simple-service
      'ocaml-packages
      home-profile-service-type
      (list ocaml-ocp-indent
            ocamlformat
            ocaml-utop
            dune
            opam
            ocaml-merlin))))

  (feature
   (name 'ocaml)
   (home-services-getter home-ocaml-services)))
