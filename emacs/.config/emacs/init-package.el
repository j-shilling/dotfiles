;;; init-package.el --- Initialize package -*- lexical-binding: t -*-

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

(require 'package)
(require 'package-vc)

(setopt package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-user-dir (init-lib-state-file "elpa")
        package-quickstart-file (init-lib-state-file "package-quickstart.el")
        package-quickstart t
        package-install-upgrade-built-in t
        package-selected-packages '(which-key
                                    vertico
                                    orderless
                                    marginalia
                                    diminish
                                    consult
                                    corfu
                                    embark
                                    embark-consult
                                    terraform-mode
                                    envrc
                                    exec-path-from-shell
                                    consult-eglot
                                    consult-eglot-embark
                                    multiple-cursors
                                    pass
                                    wgrep
                                    diff-hl
                                    ibuffer
                                    magit
                                    magit-todos
                                    forge
                                    helpful
                                    eglot-tempel
                                    tempel
                                    tempel-collection
                                    ox-gfm
                                    devdocs
                                    apheleia
                                    rbs-mode
                                    rvm
                                    all-the-icons
                                    all-the-icons-dired
                                    all-the-icons-ibuffer
                                    all-the-icons-completion
                                    ace-window
                                    avy
                                    gptel
                                    mcp
                                    mermaid-mode
                                    mermaid-ts-mode
                                    eat
                                    ob-mermaid
                                    org
                                    org-modern))

(setopt package-vc-selected-packages
        '((gptel-prompts :url "https://github.com/jwiegley/gptel-prompts.git")
          (org-modern-indent :url "https://github.com/jdtsmith/org-modern-indent.git")))

(package-initialize)

(setopt use-package-enable-imenu-support t
        use-package-hook-name-suffix nil)

(eval-when-compile
  (require 'use-package))

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(provide 'init-package)
;;; init-package.el ends here
