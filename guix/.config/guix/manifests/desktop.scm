(define-module (manifests desktop)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:export (desktop-packages
            desktop-manifest))

(define desktop-specifications
  '(
    "xsettingsd""python-pywal""polybar""dunst"
    "libnotify""rofi""emacs-exwm"
    ))

(define desktop-packages
  (append
   (map specification->package desktop-specifications)
   (list
    (@ (packages emacs-polybar) emacs-polybar)(@ (packages emacs-monitor) emacs-monitor)
    )))

(define desktop-manifest
  (packages->manifest desktop-packages))

desktop-manifest
