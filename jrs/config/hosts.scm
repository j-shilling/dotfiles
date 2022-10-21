(define-module (jrs config hosts)
  #:use-module (gnu packages)
  #:use-module (gnu system file-systems)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (rde features system))

;;
;; Special Thanks to https://github.com/nicolas-graves/dotfiles
;;

(define devices
  `(("olympus" .
     ((efi         . ,(uuid "F12D-A33C" 'fat32))
      (swap        . "/dev/nvme0n1p1")
      (root        . ,(uuid "5b3dce06-4eb1-47a3-9b33-f7aa7a2ed83c" 'ext4))
      (kernel      . ,linux)
      (initrd      . ,microcode-initrd)
      (firmware    . ,(list linux-firmware))))))

(define (lookup var)
  "Look up the value of `VAR' in `DEVICES' on the current host."
  (cdr (assoc var (cdr (assoc (gethostname) devices)))))

(define file-systems
  (list
   (file-system
     (mount-point "/boot/efi")
     (type "vfat")
     (device (lookup 'efi)))
   (file-system
     (mount-point "/")
     (type "ext4")
     (device (lookup 'root)))))

(define-public %host-features
  (list
   (feature-host-info
    #:host-name (gethostname)
    #:timezone "America/New_York")
   (feature-kernel
    #:kernel   (lookup 'kernel)
    #:initrd   (lookup 'initrd)
    #:firmware (lookup 'firmware))
   (feature-file-systems
    #:file-systems file-systems
    #:swap-devices (list (swap-space (target (lookup 'swap)))))
   (feature-bootloader)))

(define-public %live-host-features
  (list
   (feature-host-info
    #:host-name "GuixLive"
    #:timezone "America/New_York")
   (feature-kernel
    #:kernel   (lookup 'kernel)
    #:initrd   (lookup 'initrd)
    #:firmware (lookup 'firmware))
   (feature-file-systems
    #:file-systems (list (file-system
                           (mount-point "/")
                           (device (file-system-label "Guix_image"))
                           (type "ext4"))
                         (file-system
                           (mount-point "/tmp")
                           (device "none")
                           (type "tmpfs")
                           (check? #f))))
   (feature-bootloader)))
