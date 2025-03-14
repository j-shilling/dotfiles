(use-modules (guix channels))

(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix-mirror.git")
        (branch "master")
        (commit
          "df959e33ae960729a4730017e89091daf285ec9a")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
