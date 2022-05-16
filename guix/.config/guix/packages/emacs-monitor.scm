(define-module (packages emacs-monitor)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:export (emacs-monitor))

(define-public emacs-monitor
  (let ((version "0.0.1")
        (revision "1")
        (commit "e8e49cd8249fa489e2435e25a7b7c3e3e39dd115")
        (url "https://gitlab.com/e4303/packages/monitors.git")
        (homepage "https://gitlab.com/e4303/packages/monitors")
        (hash "159a35zcyyzj092jdkzaa1mwsknw33qcjfqx40k785l5l71gm22l"))
    (package
     (name "emacs-monitor")
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
      (list emacs-dash)))))

emacs-monitor
