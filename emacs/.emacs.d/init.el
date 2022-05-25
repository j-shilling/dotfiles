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

;; Doom Style Init Hooks
(require 'nadvice)
(defun init-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once after any TRIGGER-HOOKS.

This is a simplified version of `doom-run-hook-on' from Doom Emacs.

HOOK-VAR is a quoted hook.
TRIGGER-HOOKS is a list of quoted hooks."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            (when (and after-init-time
                       (or (daemonp)
                           (and (boundp hook)
                                (symbol-value hook))))
              (run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

(defvar init-first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'init-first-input-hook 'permanent-local t)

(defvar init-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'init-first-file-hook 'permanent-local t)

(defvar init-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'init-first-buffer-hook 'permanent-local t)

(unless noninteractive
  (init-run-hook-on 'init-first-buffer-hook '(find-file-hook window-buffer-change-functions))
  (init-run-hook-on 'init-first-file-hook '(find-file-hook dired-initial-position-hook))
  (init-run-hook-on 'init-first-input-hook '(pre-command-hook)))

;; Config Directories
(require 'xdg)
(defun init-data-home ()
  "Find the directory for user specific data files."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "APPDATA")
     (xdg-data-home))))

(defun init-state-home ()
  "Find the directory for user specific data files.

This directory is for files less portable or less important than
the ones in `init-data-home'."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg--dir-home "XDG_STATE_HOME" "~/.local/state"))))

(defun init-cache-home ()
  "Find the directory for user specific cache files."
  (expand-file-name
   "emacs"
   (if IS-WINDOWS
       (getenv "LOCALAPPDATA")
     (xdg-cache-home))))

;; Path Utility Functions

(require 'cl-lib)
(defun init-path (&rest segments)
  "Assemble SEGMENTS into a complete path string."
  (cl-reduce (lambda (acc segment)
               (expand-file-name segment acc))
             segments))

(defun init-data-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-data-home'."
  (apply #'init-path (init-data-home) segments))

(defun init-state-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-state-home'."
  (apply #'init-path (init-state-home) segments))

(defun init-cache-path (&rest segments)
  "Assemble SEGMENTS into a path relative to `init-cache-home'."
  (apply #'init-path (init-cache-home) segments))

;; Package Management

(eval-when-compile
  (defvar straight-base-dir)
  (defvar straight-use-package-by-default))
(defun init-ensure-straight ()
  "Ensure that straight is installed."
  (unless (featurep 'init-straight)
    (setq straight-base-dir (init-state-path)
          straight-use-package-by-default t)
    (let ((bootstrap-file
           (init-state-path "straight" "repos" "straight.el" "bootstrap.el")))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)
      (provide 'init-straight))))

(eval-when-compile
  (defun straight-use-package (&rest _)))
(defun init-ensure-package (package)
  "Install PACKAGE if necessary."
  (init-ensure-straight)
  (straight-use-package package))

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
       (init-cache-path "autosave")))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; Backups

(setq backup-directory-alist
      `(("." . ,(init-cache-path "backups"))))

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
        (init-cache-path "bookmark")))

;; Recentf
(eval-when-compile
  (defvar recentf-max-menu-items))
(eval-after-load 'recentf
  (setq recentf-max-menu-items 50))
(add-hook 'init-first-file-hook #'recentf-mode)

;; https://emacs.stackexchange.com/questions/4187/strip-text-properties-in-savehist
(defun unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
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
(add-hook 'init-first-input-hook #'savehist-mode)

(eval-when-compile
  (defvar save-place-file)
  (defvar save-place-forget-unreadable-files))
(eval-after-load 'saveplace
  (setq save-place-file (init-cache-path "saveplace")
        save-place-forget-unreadable-files t))
(add-hook 'init-first-input-hook #'save-place-mode)

(require 'server)
(unless (server-running-p)
  (server-start))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (init-ensure-package 'lispy)
	    (lispy-mode)))

(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message "Initialization time %.2fs (+ %.2f system time)"
           init-time (- total-time init-time)))

(provide 'init)
;;; init.el ends here
