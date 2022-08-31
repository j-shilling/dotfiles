(define-module (jrs manifests media)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (packages
            manifest))

(define packages
  (map (compose list specification->package+output)
       '("gst-plugins-good"
         "gst-plugins-bad"
         "gst-plugins-ugly")))

(define manifest
  (packages->manifest packages))

manifest
