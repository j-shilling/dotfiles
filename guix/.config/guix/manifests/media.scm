(define-module (manifests media)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix profiles)
  #:export (media-packages
            media-manifest))

(define media-packages
  (list gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly))

(define media-manifest
  (packages->manifest media-packages))

media-manifest
