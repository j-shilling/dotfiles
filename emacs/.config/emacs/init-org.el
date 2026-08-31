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

(defun init-org--org-directory-file (&rest args)
  "Return `ARGS' as a path relative to `org-directory'."
  (apply #'init-lib-parts-to-path org-directory args))

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
  (org-pretty-entities t)
  (org-ellipsis "...")
  (org-hide-emphasis-markers t)
  (org-log-into-drawer t)
  (org-default-notes-file (init-org--org-directory-file "todo.org")))

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

(use-package org-entities
  :custom
  (org-entities-user
   '(("Afr" "\\mathfrak{A}" t "&Afr;" "𝔄" "𝔄" "𝔄")
     ("Bfr" "\\mathfrak{B}" t "&Bfr;" "𝔅" "𝔅" "𝔅")
     ("Cfr" "\\mathfrak{C}" t "&Cfr;" "ℭ" "ℭ" "ℭ")
     ("Dfr" "\\mathfrak{D}" t "&Dfr;" "𝔇" "𝔇" "𝔇")
     ("Efr" "\\mathfrak{E}" t "&Efr;" "𝔈" "𝔈" "𝔈")
     ("Ffr" "\\mathfrak{F}" t "&Ffr;" "𝔉" "𝔉" "𝔉")
     ("Gfr" "\\mathfrak{G}" t "&Gfr;" "𝔊" "𝔊" "𝔊")
     ("Hfr" "\\mathfrak{H}" t "&Hfr;" "ℌ" "ℌ" "ℌ")
     ("Ifr" "\\mathfrak{I}" t "&Ifr;" "ℑ" "ℑ" "ℑ")
     ("Jfr" "\\mathfrak{J}" t "&Jfr;" "𝔍" "𝔍" "𝔍")
     ("Kfr" "\\mathfrak{K}" t "&Kfr;" "𝔎" "𝔎" "𝔎")
     ("Lfr" "\\mathfrak{L}" t "&Lfr;" "𝔏" "𝔏" "𝔏")
     ("Mfr" "\\mathfrak{M}" t "&Mfr;" "𝔐" "𝔐" "𝔐")
     ("Nfr" "\\mathfrak{N}" t "&Nfr;" "𝔑" "𝔑" "𝔑")
     ("Ofr" "\\mathfrak{O}" t "&Ofr;" "𝔒" "𝔒" "𝔒")
     ("Pfr" "\\mathfrak{P}" t "&Pfr;" "𝔓" "𝔓" "𝔓")
     ("Qfr" "\\mathfrak{Q}" t "&Qfr;" "𝔔" "𝔔" "𝔔")
     ("Rfr" "\\mathfrak{R}" t "&Rfr;" "ℜ" "ℜ" "ℜ")
     ("Sfr" "\\mathfrak{S}" t "&Sfr;" "𝔖" "𝔖" "𝔖")
     ("Tfr" "\\mathfrak{T}" t "&Tfr;" "𝔗" "𝔗" "𝔗")
     ("Ufr" "\\mathfrak{U}" t "&Ufr;" "𝔘" "𝔘" "𝔘")
     ("Vfr" "\\mathfrak{V}" t "&Vfr;" "𝔙" "𝔙" "𝔙")
     ("Wfr" "\\mathfrak{W}" t "&Wfr;" "𝔚" "𝔚" "𝔚")
     ("Xfr" "\\mathfrak{X}" t "&Xfr;" "𝔛" "𝔛" "𝔛")
     ("Yfr" "\\mathfrak{Y}" t "&Yfr;" "𝔜" "𝔜" "𝔜")
     ("Zfr" "\\mathfrak{Z}" t "&Zfr;" "ℨ" "ℨ" "ℨ")
     ("vdash" "\vDash" t "&vDash;" "⊨" "⊨" "⊨"))))

(use-package citar
  :if (package-installed-p 'citar)
  :custom
  (citar-bibliography (init-org--org-directory-file "references.bib")))

(use-package citar-embark
  :if (package-installed-p 'citar-embark)
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))

(provide 'init-org)
;;; init-org.el ends here
