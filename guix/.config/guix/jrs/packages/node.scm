(define-module (jrs packages node)
  #:use-module (srfi srfi-28)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) :prefix license:))

;https://nodejs.org/dist/v16.17.1/node-v16.17.1-linux-x64.tar.xz
(define-public node
  (package
   (name "node")
   (version "16.17.1")
   (source (origin (method url-fetch)
                   (uri
                    (format
                     "https://nodejs.org/dist/v~a/node-v~a-linux-x64.tar.xz"
                     version version))
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
   (inputs
    `((,gcc "lib")))
   (license license:expat)))

node
