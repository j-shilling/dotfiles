;;; config-dired.el -- Configure Dired -*- lexical-bidning: t -*-

(use-package dired
  :straight nil
  :hook
  ((dired-mode . dired-omit-mode)
   (dired-mode . dired-hide-details-mode))
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        dired-listing-switches "-lah -v --group-directories-first")
  (setq dired-omit-verbose nil
        dired-clean-confirm-killing-deleted-buffers nil))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package proced
  :straight nil
  :hook
  (proced-mode . (lambda ()
                   (proced-toggle-auto-update 1)))
  :config
  (setq proced-auto-update-interval 1))

(provide 'config-dired)
;;; config-dired.el ends here
