;;; config-ui.el --- Configure Appearance and UI behavior -*- lexical-binding: t; -*-
;;
;;; Compentary:
;;
;;; Code:

;;;
;;; Frames and Windows
;;;

(setq default-frame-alist
      '((min-heigt . 1)
        (height . 45)
        (min-width . 1)
        (width . 81)
        (vertical-scroll-bars . nil)
        (internal-border-width . 8)
        (left-fringe . 1)
        (right-fringe . 1)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq bookmark-set-fringe-mark nil)

(setq window-divider-default-right-width 8)
(add-hook 'init-first-buffer-hook #'window-divider-mode)

;;;
;;; Cleanup
;;;

;; Turn of decorations
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Avoid inconsistent GUIs
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;;;
;;; Fonts
;;;

(defun config-ui-font-exists-p (font-name)
  "Returns `t' if FONT-NAME is a valid font family."
  (member font-name (font-family-list)))

(defun config-ui-find-font ()
  (cond ((config-ui-font-exists-p "Fira Code")
         (font-spec :family "Fira Code" :size 18))
        (t nil)))

(defun config-ui-variable-pitch-font ()
  (cond ((config-ui-font-exists-p "Fira Sans")
         (font-spec :family "Fira Sans" :size 18))
        (t nil)))

(defvar config-ui-font (config-ui-find-font)
  "The default font to use.

Inspired by the way Doom Emacs handles `doom-font'.")

(defvar config-ui-variable-pitch-font (config-ui-variable-pitch-font))
(defvar config-ui-serif-font nil)

(defun config-ui-load-fonts ()
  "Loads `init-font'."
  (interactive)
  (dolist (pair `((default . ,config-ui-font)
                  (fixed-pitch . ,config-ui-font)
                  (fixed-pitch-serif . ,config-ui-serif-font)
                  (variable-pitch . ,config-ui-variable-pitch-font)))
    (when-let ((face (car pair))
               (font (cdr pair)))
      (set-face-attribute face nil
                          :width 'normal :weight 'normal
                          :slant 'normal :font font))))

(add-hook 'init-first-buffer-hook
          #'config-ui-load-fonts)

;;;
;;; Scrolling
;;;

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(add-hook 'init-first-buffer-hook #'pixel-scroll-precision-mode)

;;;
;;; Cursor
;;;

(setq x-stretch-cursor nil)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'init-first-buffer-hook (lambda ()
                                    (blink-cursor-mode -1)))

(setq-default cursor-in-non-selected-windows nil)

;;;
;;; Mode and header lines
;;;

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
(setq mode-line-format nil)

;;;
;;; Theme
;;;

(use-package modus-themes
  :hook
  (init-first-buffer . (lambda ()
                         (load-theme 'modus-vivendi t)))
  :init
  (setq modus-themes-mode-line '(borderless)
        modus-themes-diffs 'desaturated
        modus-themes-deuteranopia t
        modus-themes-fringes nil

        modus-themes-operandi-color-overrides
        '((fg-window-divider-inner . "#ffffff")
          (fg-window-divider-outer . "#ffffff"))
        modus-themes-vivendi-color-overrides
        '((fg-window-divider-inner . "#000000")
          (fg-window-divider-outer . "#000000"))))

(provide 'config-ui)
;;; config-ui.el ends here
