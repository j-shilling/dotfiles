(define-module (packages emacs-polybar)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (packages emacs-monitor)
  #:export (emacs-polybar))

(define-public emacs-polybar
  (let ((version "0.0.1")
        (revision "1")
        (commit "3889d3c23f22cb939ea0c34a97b77d3fe31fe7d0")
        (url "https://gitlab.com/e4303/packages/polybar.el.git")
        (homepage "https://gitlab.com/e4303/packages/polybar.el")
        (hash "00r2wlpbrz5r1rm6sn4hcdgh2bvv3vq04f94skwxj0wila4bgfxw"))
    (package
     (name "emacs-polybar")
     (version (git-version version revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url url)
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 hash))))
     (build-system emacs-build-system)
     (home-page homepage)
     (license gpl3+)
     (synopsis "")
     (description "")
     (propagated-inputs
      (list emacs-dash
            emacs-monitor
            emacs-a
            emacs-ivy)))))

emacs-polybar
