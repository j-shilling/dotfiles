(define-module (jrs packages mozilla)
  #:use-module ((nongnu packages mozilla) :prefix nongnu:)
  #:use-module (guix packages)
  #:use-module ((gnu packages linux) :prefix gnu:))

(define pipewire gnu:pipewire-0.3)

(define-public firefox
  (package
    (inherit nongnu:firefox)
    (inputs
     (modify-inputs (package-inputs nongnu:firefox)
                    (replace "pipewire" pipewire)))))

(define-public firefox/wayland
  (package
    (inherit nongnu:firefox/wayland)
    (inputs
     (modify-inputs (package-inputs nongnu:firefox/wayland)
                    (replace "pipewire" pipewire)))))
