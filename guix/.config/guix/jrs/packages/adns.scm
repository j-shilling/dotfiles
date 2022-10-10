(define-module (jrs packages adns)
  #:use-module ((gnu packages adns) :prefix adns:)
  #:use-module (guix packages))

(define-public c-ares
  (package
    (inherit adns:c-ares)
    (version "1.18.1")))

c-ares
