(use-modules
 (gnu home)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services shells)
 (guix profiles)
 (jake manifests doom-emacs-packages)
 (jake manifests general-utilities)
 (jake manifests media-packages))

(home-environment
 (packages
  (append general-utilities media-packages doom-emacs-packages))
  (services
    (list (service
            home-bash-service-type
            (home-bash-configuration
              (aliases
                '(("grep" . "grep --color=auto")
                  ("ll" . "ls -l")
                  ("ls" . "ls -p --color=auto")))
              (bashrc
                (list (local-file
                        "/home/jake/src/guix-config/.bashrc"
                        "bashrc")))
              (bash-profile
                (list (local-file
                        "/home/jake/src/guix-config/.bash_profile"
                        "bash_profile"))))))))
