(define-module (jrs packages linux)
  #:use-module ((gnu packages linux) :prefix gnu:)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public pipewire
  (package
   (inherit gnu:pipewire-0.3)
   (version "0.3.59")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/PipeWire/pipewire")
                  (commit version)))
            (file-name (git-file-name (package-name gnu:pipewire-0.3) version))
            (sha256
             (base32
              "17jnj1las9wnizwh6g9ix2pxnna029vj66qwhrnij19315vfs073"))))))

(define-public wireplumber
  (package
   (inherit gnu:wireplumber)
   (version "0.4.12")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url
            "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
           (commit version)))
     (file-name (git-file-name (package-name gnu:wireplumber) version))
     (sha256
      (base32 "0k6ylqq4r1xzc7bbmwy1qgwpqra8shdvilwm1pvzzvilgyqkm8ys"))))
   (inputs
    (modify-inputs (package-inputs gnu:wireplumber)
                   (replace "pipewire" pipewire)))))

wireplumber
