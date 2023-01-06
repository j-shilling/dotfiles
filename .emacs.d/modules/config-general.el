;;; config-general.el --- General Configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;;
;;; Encoding
;;;

(require 'seq)
(seq-doseq (fn '(set-default-coding-systems
                 prefer-coding-system
                 set-terminal-coding-system
                 set-keyboard-coding-system
                 set-buffer-file-coding-system
                 set-selection-coding-system))
  (apply fn (list (if IS-WINDOWS
                      'utf-8-dos
                    'utf-8-unix))))
(set-language-environment "English")

;;;
;;; Tabs
;;;

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(add-hook 'init-first-input-hook #'delete-selection-mode)

(setq global-auto-revert-non-file-buffers t)
(add-hook 'init-first-file-hook #'global-auto-revert-mode)

(use-package whitespace
  :straight nil
  :custom
  (whitespace-action '(cleanup auto-cleanup)))

;;;
;;; Backups and Recovery
;;;

(setq auto-save-list-file-prefix
      (file-name-as-directory
       (init-cache-path "autosave")))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq backup-directory-alist
      `(("." . ,(init-cache-path "backups"))))

(setq make-backup-files t
      vc-make-backup-files nil
      backup-by-copying t
      version-control t
      kept-old-versions 6
      kept-new-versions 9
      delete-by-moving-to-trash nil)

;;;
;;; Bookmarks and Recentf
;;;

(use-package bookmark
  :straight nil
  :init
  (setq bookmark-default-file
        (init-cache-path "bookmark")))

(use-package recentf
  :straight nil
  :hook
  ((init-first-buffer . recentf-mode)
   (recentf-mode . (lambda ()
                     (run-with-idle-timer 30 t 'recentf-save-list))))
  :init
  (setq recentf-max-menu-items 50
        recentf-save-file (init-cache-path "recentf")))
;;;
;;; Save History
;;;

;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :straight nil
  :hook
  (init-first-input . savehist-mode)
  :init
  (setq kill-ring-max 50
        kill-do-not-save-duplicates t
        history-length 50
        savehist-file (init-cache-path "history"))
  (setq savehist-additional-variables
        '(kill-ring
          command-history
          set-variable-value-history
          query-replace-history
          read-expression-history
          minibuffer-history
          read-char-history
          face-name-history
          bookmark-history
          file-name-history))
  (put 'minibuffer-history 'history-length 50)
  (put 'file-name-history 'history-length 50)
  (put 'set-variable-value-history 'history-length 25)
  (put 'query-replace-history 'history-length 25)
  (put 'read-expression-history 'history-length 25)
  (put 'read-char-history 'history-length 25)
  (put 'face-name-history 'history-length 25)
  (put 'bookmark-history 'history-length 25)
  (setq history-delete-duplicates t))

(use-package saveplace
  :straight nil
  :hook
  (init-first-input . save-place-mode)
  :init
  (setq save-place-file (init-cache-path "places")
        save-place-forget-unreadable-files t))

;;;
;;; Prompts
;;;

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;;
;;; Long Lines and Large Files
;;;

(setq vc-follow-symlinks t)

(setq large-file-warning-threshold nil)

(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

(add-hook 'init-first-buffer-hook
          (lambda ()
            (global-so-long-mode 1)))

;;;
;;; Help
;;;

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

;;;
;;; Spelling
;;;

(use-package consult-flyspell)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

;;;
;;; Managing Windows
;;;

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t
        aw-ignore-current nil))

;;;
;;; IBuffer
;;;

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold))
        ibuffer-formats
        `((mark modified read-only locked
                ,(propertize " " 'display `(space :align-to 8))
                (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 :left)))
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))

  (define-ibuffer-column icon (:name "  ")
    (let ((icon (if (and (buffer-file-name)
                         (all-the-icons-auto-mode-match?))
                    (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

(use-package ibuffer-vc)

;;;
;;; Misc
;;;

(add-hook 'init-first-buffer-hook #'subword-mode)
(add-hook 'init-first-buffer-hook #'column-number-mode)

(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)
(setq require-final-newline t)

(provide 'config-general)
;;; config-general.el ends here
