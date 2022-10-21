(define-module (jrs config profile)
  #:use-module (gnu packages))

(define-public %home-packages
  (map specification->package+output
          '("bluez"
            "flatpak"
            "make"
            "nyxt"
            "ungoogled-chromium"
            "zathura"
            "zathura-pdf-poppler"
            "qemu")))

(define-public %system-packages
  (map specification->package+output
       '("nss-certs")))
