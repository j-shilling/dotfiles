;;; init-appearance.el --- Configure appearance -*- lexical-binding: t -*-
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:
(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

(use-package emacs
  :init
  (set-default 'cursor-type '(bar . 1))
  (setq-default cursor-in-non-selected-windows nil)
  (set-frame-parameter (selected-frame) 'internal-border-width 8)
  (setq window-divider-default-right-width 8)

  (when (init-lib-mac-p)
    (set-frame-font "JetBrains Mono 14")
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  :hook
  ((after-init-hook . window-divider-mode)))

(use-package menu-bar
  :config
  (menu-bar-mode 0))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package fringe
  :config
  (set-fringe-mode 8))

(use-package fontaine
  :if (package-installed-p 'fontaine)
  :custom
  (fontaine-latest-state-file
   (init-lib-state-file "fontaine.eld"))
  (fontaine-presets
   '((regular
      :default-height 150)
     (large
      :default-weight semilight
      :default-height 180
      :bold-weight extrabold)
     (t ; our shared fallback properties
      :default-family "Iosevka"
      :default-weight normal
      ;; :default-height 100
      :fixed-pitch-family nil ; falls back to :default-family
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka Aile"
      :variable-pitch-weight normal
      :variable-pitch-height 1.05
      :bold-family nil ; use whatever the underlying face has
      :bold-weight bold
      :italic-family nil
      :italic-slant italic
      :line-spacing nil)))
  :bind (("C-c f" . fontaine-set-preset))
  :hook ((after-init-hook . fontaine-mode)))

(use-package all-the-icons-ibuffer
  :if (package-installed-p 'all-the-icons-ibuffer)
  :hook
  ((ibuffer-mode-hook . all-the-icons-ibuffer-mode)))

(use-package all-the-icons-completion
  :if (and (package-installed-p 'all-the-icons-completion)
           (package-installed-p 'marginalia))
  :hook
  ((marginalia-mode-hook . all-the-icons-completion-marginalia-setup)))

(use-package all-the-icons-completion
  :if (and (package-installed-p 'all-the-icons-completion)
           (not (package-installed-p 'marginalia)))
  :hook
  ((after-init-hook . all-the-icons-completion-mode)))

(use-package pixel-scroll
  :diminish pixel-scroll-precision-mode
  :hook
  ((after-init-hook . pixel-scroll-precision-mode)))

(use-package modus-themes
  :defines
  modus-themes-mode-line
  modus-themes-diffs
  modus-themes-deuteranopia
  modus-themes-fringes
  :init
  (load-theme 'modus-vivendi t)
  :config
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t))

(use-package which-key
  :if (package-installed-p 'which-key)
  :diminish which-key-mode
  :hook
  (after-init-hook . which-key-mode))

(provide 'init-appearance)
;;; init-appearance.el ends here
