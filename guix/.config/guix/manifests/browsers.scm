(define-module (manifests browsers)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("nyxt"
         "firefox"
         "ungoogled-chromium")))

(define manifest
  (packages->manifest packages))

manifest
