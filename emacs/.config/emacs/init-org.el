;;; init-org.el --- Configure org-mode and related packages -*- lexical-binding: t -*-

;; Author: Jake Shilling

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

(use-package org
  :custom
  (org-clock-persist-file           (init-lib-cache-file "org/clock-persist.el"))
  (org-id-locations-file            (init-lib-cache-file "org/id-locations.el"))
  (org-persist-directory            (init-lib-cache-file "org/persist/"))
  (org-publish-timestamp-directory  (init-lib-cache-file "org/timestamps/"))

  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-adapt-indentation nil)
  (org-startup-indented nil)
  (org-ellipsis "⤵")
  (org-hide-emphasis-markers t)
  (org-log-into-drawer t)
  (org-default-notes-file (concat org-directory "/todo.org")))

(use-package org-refile
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'full-file-path)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets `((nil . (:maxlevel . 3))
                        (org-agenda-files . (:maxlevel . 3)))))

(use-package org-capture
  :bind
  (:map mode-specific-map
        ("c" . org-capture)))

(use-package org-modern-indent
  :if (package-installed-p 'org-modern-indent)
  :hook
  ((org-mode-hook . org-modern-indent-mode)))

(use-package org-modern
  :if (package-installed-p 'org-modern)
  :after (org)
  :config
  (global-org-modern-mode))

(provide 'init-org)
;;; init-org.el ends here
