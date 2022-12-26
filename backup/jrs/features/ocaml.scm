(define-module (jrs features ocaml)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages texinfo)
  #:use-module (jrs packages ocaml-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:export (feature-ocaml))

(define (feature-ocaml)

  (define (home-ocaml-services config)
    (list
     (rde-elisp-configuration-service
      'ocaml
      config
      '(;; Tuareg installed through Guix
        (add-to-list 'auto-mode-alist
                     '("\\.ml\\'" . tuareg-mode))
        (with-eval-after-load
         'tuareg
         (setq tuareg-prettify-symbols-full t
               tuareg-opam-insinuate t)
         (tuareg-opam-update-env (tuareg-opam-current-compiler))
         (add-hook 'tuareg-mode-hook 'ocp-setup-indent)
         (add-hook 'tuareg-mode-hook 'utop-minor-mode))

        ;; Other ocaml packages installed through opam. Add the load path
        (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
          (when (and opam-share (file-directory-p opam-share))
            ;; Register Merlin
            (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
            (autoload 'merlin-mode "merlin" nil t nil)
            ;; Automatically start it in OCaml buffers
            (add-hook 'tuareg-mode-hook 'merlin-mode t)
            (add-hook 'caml-mode-hook 'merlin-mode t)
            ;; Use opam switch to lookup ocamlmerlin binary
            (setq merlin-command 'opam)
            (autoload 'ocp-setup-indent "ocp-indent" nil t)))

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

     (when (get-value 'zsh config)
       (simple-service
        'opam-zsh-init
        home-zsh-service-type
        (home-zsh-extension
         (zshrc
          (list
           "test -r ${HOME}/.opam/opam-init/init.sh && . ${HOME}/.opam/opam-init/init.sh")))))

     (when (get-value 'bash config)
       (simple-service
        'opam-bash-init
        home-bash-service-type
        (home-bash-extension
         (bashrc
          (list
           "test -r ${HOME}/.opam/opam-init/init.sh && . ${HOME}/.opam/opam-init/init.sh")))))

     (simple-service
      'ocaml-packages
      home-profile-service-type
      (list opam
            ocaml-manual
            ;; Packages need for opam to build ocaml
            gcc
            glibc
            binutils
            libx11
            libiberty
            zlib
            perl
            pkg-config))))

  (feature
   (name 'ocaml)
   (home-services-getter home-ocaml-services)))
