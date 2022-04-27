;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(home-environment
  (packages
    (map (compose list specification->package+output)
         (list "zsh"
               "cmake"
               "curl"
               "sbcl"
               "nyxt"
               "emacs"
               "stow"
               "git"
               "pinentry"
               "gnupg@2.2.27"
               "openssh")))
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
