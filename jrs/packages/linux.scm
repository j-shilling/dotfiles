(define-module (jrs packages linux)
  #:use-module ((gnu packages linux) :prefix gnu:)
  #:use-module (guix packages))

(define-public pipewire
  (package
   (inherit gnu:pipewire-0.3)
   (version "0.3.59")))

(define-public wireplumber
  (package
   (inherit gnu:wireplumber)
   (version "0.4.12")
   (inputs
    (modify-inputs (package-inputs gnu:wireplumber)
                   (replace "pipewire" pipewire)))))

wireplumber
