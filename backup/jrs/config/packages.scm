(define-module (jrs config profile)
  #:use-module (gnu packages))

(define-public %home-packages
  (map specification->package+output
          '("bluez"
            "zoom"
            "flatpak"
            "make"
            "nyxt"
            "ungoogled-chromium"
            "zathura"
            "zathura-pdf-poppler")))

(define-public %system-packages
  (map specification->package+output
          '("nss-certs")))
