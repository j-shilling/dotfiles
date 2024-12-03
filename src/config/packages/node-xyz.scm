(define-module (config packages node-xyz)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public devcontainers-cli-0.72.0
  (package
   (name "devcontainers-cli")
   (version "0.72.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@devcontainers/cli/-/cli-0.72.0.tgz")
     (sha256
      (base32
       "1nq48y509gmny97k47wnkaph9wlqz1ilk3grc0vf0s9q6d85bsfw"))))
   (build-system node-build-system)
   (arguments
    `(#:tests? #f))
   (home-page "https://github.com/devcontainers/cli#readme")
   (synopsis "")
   (description "")
   (license license:expat)))