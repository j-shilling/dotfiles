(define-module (jake manifests media-packages)
  #:use-module (gnu packages)
  #:export (media-packages))

(define media-packages
  (map
   (lambda (spec) (specification->package spec))
   '("gst-plugins-good"
     "gst-plugins-bad"
     "gst-plugins-ugly")))

media-packages
