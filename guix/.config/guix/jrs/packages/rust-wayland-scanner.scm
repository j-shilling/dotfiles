(define-module (jrs packages rust-wayland-scanner)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io))

(define-public rust-wayland-scanner
  (package
   (inherit rust-wayland-scanner-0.28)
   (inputs (modify-inputs (package-inputs rust-wayland-scanner-0.28)
                          (append rust-unicode-ident-1)))))

rust-wayland-scanner
