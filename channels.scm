(use-modules (guix ci)
             (guix channels))

(list
 (channel
  (name 'guix)
  (url "https://codeberg.org/guix/guix-mirror.git")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
   (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D E643 A2A0 6DF2 A33A 54FA"))))
 ;; (channel
 ;;  (name 'guix)
 ;;  (url "https://git.savannah.gnu.org/git/guix.git")
 ;;  (introduction
 ;;   (make-channel-introduction
 ;;    "9edb3f66fd807b096b48283debdcddccfea34bad"
 ;;   (openpgp-fingerprint
 ;;     "BBB0 2DDF 2CEA F6A8 0D1D E643 A2A0 6DF2 A33A 54FA"))))
 )
