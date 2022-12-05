(define-module (jrs features mako)
  #:use-module (gnu services)
  #:use-module (gnu packages wm)
  #:use-module (rde features)
  #:use-module (jrs services mako)
  #:use-module (rde features fontutils)
  #:export (feature-mako))

(define* (feature-mako
          #:key
          (mako mako))

  (define (home-mako-services config)

    (define font-sans
      (let* ((font (get-value 'font-sans config))
             (name (font-name font))
             (size (font-size font)))
        (format #f "~a ~a" name size)))

    (list
     (service
      home-mako-service-type
      (home-mako-configuration
       (package mako)
       (config
        `((font . "Fira Code 18")
          (background-color . "#6f6f6f")
          (text-color . "#FFFFFFFF")
          (width . 300)
          (height . 100)
          (margin . 10)
          (padding . 5)
          (border-color . "#4d4d4d")
          (border-size . 4)
          (border-radius . 10)
          (progress-color . "over #1a1a1a")
          (icons . 1)
          (max-icon-size . 64)
          (icon-location . left)
          (markup . 1)
          (actions . 1)
          (history . 1)
          (format . "<b>%s</b>\\n%b")
          (text-alignment . left)
          (default-timeout . 1000)
          (ignore-timeout . 0)
          (max-visible . 5)
          (layer . overlay)
          (anchor . top-right)))))))

  (feature
   (name 'mako)
   (home-services-getter home-mako-services)))
