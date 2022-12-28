;;; config-org.el -- General Configuration -*- lexical-binding: t; -*-

(use-package org
  :init
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-startup-indented t
        org-log-done t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-log-into-drawer t
        org-clock-report-include-clocking-task t
        org-clock-out-when-done t
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist t
        org-clock-in-resume t
        org-clock-persist-query-resume nil
        org-modules '(org-habit))

  (setq org-ellipsis " ↓ "
        org-pretty-entities t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(provide 'config-org)
;;; config-org.el ends here
