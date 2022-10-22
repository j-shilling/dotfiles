(define-module (jrs config profile)
  #:use-module (jrs packages polymc)
  #:use-module (gnu packages))

(define-public %home-packages
  (append
   (map specification->package+output
        '("bluez"
          "flatpak"
          "make"
          "nyxt"
          "ungoogled-chromium"
          "zathura"
          "zathura-pdf-poppler"
          "qemu"))
   (list
    polymc)))

(define-public %system-packages
  (map specification->package+output
       '("nss-certs")))
