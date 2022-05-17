;;;  init.el --- emacs startup file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defvar init-start-time (current-time))

;; Environment constants
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Config Directories
(require 'xdg)
(require 'cl-lib)

(defun config-data-home ()
  ""
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "APPDATA")
     (xdg-data-home))))

(defun config-state-home ()
  ""
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg-state-home))))

(defun config-cache-home ()
  ""
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg-cache-home))))

(defun config-path (&rest segments)
  ""
  (cl-reduce (lambda (acc segment)
               (expand-file-name segment acc))
             segments))

(defun config-cache-path (&rest segments)
  ""
  (apply #'config-path (config-cache-home) segments))

;; Startup UI

(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 initial-buffer-choice t)

;; Encoding

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

;; Recovery

(setq auto-save-list-file-prefix
      (file-name-as-directory
       (config-cache-path "autosave")))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; Backups

(setq backup-directory-alist
      `(("." . ,(config-cache-path "backups"))))

(setq make-backup-files t
      vc-make-backup-files nil
      backup-by-copying t
      version-control t
      kept-old-versions 6
      kept-new-versions 9
      delete-by-moving-to-trash t)

;; Bookmarks
(eval-when-compile
    (defvar bookmark-default-file))
(eval-after-load 'bookmark
  (setq bookmark-default-file
        (config-cache-path "bookmark")))

;; Recentf
(eval-when-compile
  (defvar recentf-max-menu-items))
(eval-after-load 'recentf
  (setq recentf-max-menu-items 50))
;; TODO: lazy init
(recentf-mode 1)

;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(eval-when-compile
  (defvar savehist-additional-variables))
(eval-after-load 'savehist
  (progn
    (setq kill-ring-max 50
          history-length 50)
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
    (setq history-delete-duplicates t)))
;; TODO: make lazy
(savehist-mode 1)

(eval-when-compile
  (defvar save-place-file)
  (defvar save-place-forget-unreadable-files))
(eval-after-load 'saveplace
  (setq save-place-file (config-cache-path "saveplace")
        save-place-forget-unreadable-files t))
;; TODO: make lazy
(save-place-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message "Initialization time %.2fs (+ %.2f system time)"
           init-time (- total-time init-time)))

(provide 'init)
;;; init.el ends here
