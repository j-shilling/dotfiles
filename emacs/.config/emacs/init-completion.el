;;; init-completion.el --- Initialize minibuffer / CAPF configuration -*- lexical-binding: t -*-

;; Author: Jake Shilling
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

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

;; commentary

;;; Code:
(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;
;;; Minibuffer
;;;;;;;;;;;;;;;;;;

(use-package minibuffer
  :custom
  (completion-cycle-threshold nil)
  (enable-recursive-minibuffers t)
  (tab-always-indent 'complete)
  (minibuffer-prompt-properties
   '(readonly t cursor-intagible t face minibuffer-prompt))
  :hook
  ((minibuffer-setup-hook . cursor-intangible-mode)))

;; Fallback to fido if vertico is not yet installed
(use-package icomplete
  :unless (package-installed-p 'vertico)
  :hook
  ((after-init-hook . fido-mode)))

;; VERTICO AND FRIENDS

;; TODO: look into vertico extensions
(use-package vertico
  :if (package-installed-p 'vertico)
  :diminish vertico-mode
  :bind
  (:map vertico-map
        ("M-?" . minibuffer-completion-help)
        ("M-TAB" . minibuffer-complete))
  :hook
  ((after-init-hook . vertico-mode)))

(use-package vertico-multiform
  :if (package-installed-p 'vertico)
  :diminish vertico-multiform-mode
  :defines (vertico-multiform-categories vertico-multiform-commands)
  :hook (vertico-mode-hook vertico-multiform-mode)
  :init
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          (info-menu buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-lsp-symbols buffer)
          (consult-xref buffer)
          (embark-keybinding buffer)
          (consult-location buffer))
        vertico-multiform-commands
        '((telega-chat-with buffer)
          (magit:--author flat)
          (Info-goto-node buffer)
          (info-lookup-symbol buffer)
          (Info-follow-reference buffer)
          (consult-yank-pop buffer))))

(use-package orderless
  :if (package-installed-p 'orderless)
  :hook
  ((after-init-hook .
                    (lambda (&rest _)
                      (require 'orderless)
                      (setq completion-styles '(orderless basic))
                      (setq completion-category-overrides
                            '((project-file (styles . (partial-completion basic orderless)))
                              (file (styles . (partial-completion basic orderless)))))))))

(use-package marginalia
  :if (package-installed-p 'marginalia)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook
  ((after-init-hook . marginalia-mode)))

(use-package consult
  :if (package-installed-p 'consult)
  :functions (consult-register-window consult-register-format)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("C-h i" . consult-info)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-ripgrep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))

  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-imenu
  :if (package-installed-p 'consult)
  :bind
  (("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)))

;;;;;;;;;;;;;;;;;;
;;; In buffer
;;;;;;;;;;;;;;;;;;

(use-package corfu
  :if (package-installed-p 'corfu)
  :diminish global-corfu-mode
  :custom
  (corfu-auto t)
  (corfu-quite-no-match 'separator)
  :hook
  ((after-init-hook . global-corfu-mode)))

(use-package corfu-history
  :if (package-installed-p 'corfu)
  :diminish corfu-history-mode
  :hook
  ((corfu-mode-hook . corfu-history-mode)))

(use-package corfu-info
  :if (package-installed-p 'corfu)
  :defines corfu-mode-map
  :bind
  (:map corfu-mode-map
        ("M-g" . corfu-info-location)
        ("M-h" . corfu-info-documentation)))

(use-package corfu-popupinfo
  :if (package-installed-p 'corfu)
  :diminish corfu-popupinfo-mode
  :hook
  (corfu-mode-hook corfu-popupinfo-mode))

(provide 'init-completion)
;;; init-completion.el ends here
