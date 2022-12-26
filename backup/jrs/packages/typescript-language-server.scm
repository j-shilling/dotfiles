(define-module (jrs packages typescript-language-server)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (jrs build npm-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public typescript-language-server
  (package
   (name "typescript-language-server")
   (version "2.1.0")
   (source (origin (method npm-fetch)
                   (uri (npm-reference
                         (name name)
                         (version version)))
                   (sha256
                    (base32
                     "1143v9zjw2pq6cmqksncdhpj30rzfm27mj2qbxzrd1d39arjxfh6"))))
   (home-page "https://nodejs.org")
   (synopsis "Evented I/O for V8 JavaScript")
   (description
    "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
   (build-system copy-build-system)
   (arguments
    '(#:validate-runpath? #f
      #:install-plan
      '(("./bin/" "bin/")
        ("./include/" "include/")
        ("./lib/" "lib/")
        ("./share/" "share/"))))

   (license license:expat)))

typescript-language-server
