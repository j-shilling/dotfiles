(use-modules (guix channels))

(list (channel
        (name 'my)
        (url "file:///home/jake/dotfiles")
        (branch "master")
        (commit
          "020d5cd9475be9917c53681914c5b47a7c3abe68"))
      (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix-mirror.git")
        (branch "master")
        (commit
          "f9726d5498e63a433fdd3398a4439089072482d5")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
