;;; config-gpg.el -- GPG Configuration -*- lexical-binding: t; -*-

(use-package epa
  :straight nil
  :config
  (setq epa-pinentry-mode 'loopback
        epa-file-select-keys nil
        epa-file-encrypt-to "0FCC8E6A96FF109F"
        epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save t))

(use-package pinentry
  :hook
  (after-init . pinentry-start))

(when (executable-find "gpgconf")
  (setenv "SSH_AUTH_SOCK"
          (string-trim
           (shell-command-to-string
            "gpgconf --list-dirs agent-ssh-socket"))))

(provide 'config-gpg)
;;;config-gpg.el ends here
