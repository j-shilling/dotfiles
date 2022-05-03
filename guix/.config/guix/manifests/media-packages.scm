(define-module (manifests media-packages)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (media-specifications
            media-packages
            media-manifest))

(define media-specifications '("gst-plugins-good"
                               "gst-plugins-bad"
                               "gst-plugins-ugly"))

(define media-packages
  (map
   (lambda (spec) (specification->package spec))
   media-specifications))

(define media-manifest
  (packages->manifest media-packages))

media-manifest
