(define-module (my packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-codeium
  (let ((commit "08d5ecfa74d960cf18af46c2d7fa0449d789d73b"))
    (package
     (name "emacs-codium")
     (version "1.12.0")
     (home-page "www.codeium.com")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/Exafunction/codeium.el.git")
              (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1mkih6gdc4w6pnk2f27blrvav3zz43qnmbdjdc7ina09cfhajw1n"))))
     (build-system emacs-build-system)
     (synopsis "Free, ultrafast Copilot alternative for Emacs")
     (description "Codeium autocompletes your code with AI in all major IDEs. We launched
this implementation of the Codeium plugin for Emacs to bring this
modern coding superpower to more developers. Check out our playground
if you want to quickly try out Codeium online.

codeium.el provides a completion-at-point-functions backend. It is
designed to be use with a front-end, such as company-mode, corfu, or
the built-in completion-at-point.

codeium.el is an open source client and (mostly) written by Alan Chen.
It uses a proprietary language server binary, currently
downloaded (automatically, with confirmation) from here. Use M-x
codeium-diagnose to see apis/fields that would be sent to the local
language server, and the command used to run the local language
server. Customize codeium-api-enabled, codeium-fields-regexps and
codeium-command to change them.")
     (license license:expat))))
