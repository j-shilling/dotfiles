;; [[file:config.org::*File Header][File Header:1]]
;;;  init.el --- emacs startup file -*- lexical-binding: t; -*-
;; File Header:1 ends here

;; [[file:config.org::*Initialize Benchmarking][Initialize Benchmarking:1]]
(defvar init-start-time (current-time))
;; Initialize Benchmarking:1 ends here

;; [[file:config.org::*Environment constants][Environment constants:1]]
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defconst LINUX-DISTRIBUTION
  (when IS-LINUX
    (or (when (file-exists-p "/etc/os-release")
          (with-temp-buffer
            (insert-file-contents "/etc/os-release")
            (goto-char (point-min) )
            (when-let ((pos (search-forward "NAME" nil t)))
              (goto-char pos)
              (string-trim
               (cadr
                (split-string
                 (string-trim (thing-at-point 'line))
                 "="))
               "\""
               "\""))))
        (string-trim (shell-command-to-string "uname -o")))))

(defconst IS-GUIX
  (string= LINUX-DISTRIBUTION "Guix System"))
;; Environment constants:1 ends here

;; [[file:config.org::*Custom Initialization Hooks][Custom Initialization Hooks:1]]
(defun init-first-buffer-h (&rest _)
  (run-hooks 'init-first-buffer-hook)
  (set 'init-first-buffer-hook nil))
(add-hook 'window-buffer-change-functions
	  #'init-first-buffer-h)
(advice-add 'after-find-file :before
	    #'init-first-buffer-h)

(defun init-first-file-h  (&rest _)
  (run-hooks 'init-first-file-hook)
  (set 'init-first-file-hook nil))
(add-hook 'dired-initial-position-hook
	  #'init-first-file-h)
(advice-add 'after-find-file :before
	    #'init-first-file-h)

(defun init-first-input-h (&rest _)
  (run-hooks 'init-first-input-hook)
  (set 'init-first-input-hook nil))
(add-hook 'pre-command-hook
	  #'init-first-input-h)
;; Custom Initialization Hooks:1 ends here

;; [[file:config.org::*Custom Initialization Hooks][Custom Initialization Hooks:2]]
(defvar init-first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'init-first-input-hook 'permanent-local t)

(defvar init-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'init-first-file-hook 'permanent-local t)

(defvar init-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'init-first-buffer-hook 'permanent-local t)
;; Custom Initialization Hooks:2 ends here

;; [[file:config.org::*Custom Initialization Hooks][Custom Initialization Hooks:3]]
(add-hook 'init-first-buffer-hook
	  (lambda () (message "first buffer hook")))
(add-hook 'init-first-file-hook
	  (lambda ()  (message "first file hook")))
(add-hook 'init-first-input-hook
	  (lambda () "first input hook"))
;; Custom Initialization Hooks:3 ends here

;; [[file:config.org::*XDG Directories][XDG Directories:1]]
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
;; XDG Directories:1 ends here

;; [[file:config.org::*XDG Directories][XDG Directories:2]]
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
;; XDG Directories:2 ends here

;; [[file:config.org::*Package Management][Package Management:1]]
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
;; Package Management:1 ends here

;; [[file:config.org::*Package Management][Package Management:2]]
(eval-when-compile
  (defun straight-use-package (&rest _)))
(defun init-ensure-package (package)
  "Install PACKAGE if necessary."
  (unless IS-GUIX
    (init-ensure-straight)
    (straight-use-package package)))
;; Package Management:2 ends here

;; [[file:config.org::*Startup UI][Startup UI:1]]
(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 initial-buffer-choice t)
;; Startup UI:1 ends here

;; [[file:config.org::*Encoding][Encoding:1]]
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
;; Encoding:1 ends here

;; [[file:config.org::*Recovery][Recovery:1]]
(setq auto-save-list-file-prefix
      (file-name-as-directory
       (init-cache-path "autosave")))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)
;; Recovery:1 ends here

;; [[file:config.org::*Backups][Backups:1]]
(setq backup-directory-alist
      `(("." . ,(init-cache-path "backups"))))

(setq make-backup-files t
      vc-make-backup-files nil
      backup-by-copying t
      version-control t
      kept-old-versions 6
      kept-new-versions 9
      delete-by-moving-to-trash t)
;; Backups:1 ends here

;; [[file:config.org::*Bookmarks][Bookmarks:1]]
(eval-when-compile
    (defvar bookmark-default-file))
(eval-after-load 'bookmark
  (setq bookmark-default-file
        (init-cache-path "bookmark")))
;; Bookmarks:1 ends here

;; [[file:config.org::*Recent Files][Recent Files:1]]
;; Recentf
(eval-when-compile
  (defvar recentf-max-menu-items))
(eval-after-load 'recentf
  (setq recentf-max-menu-items 50))
(add-hook 'init-first-file-hook #'recentf-mode)
;; Recent Files:1 ends here

;; [[file:config.org::*Save History][Save History:1]]
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
;; Save History:1 ends here

;; [[file:config.org::*Save Place in File][Save Place in File:1]]
(eval-when-compile
  (defvar save-place-file)
  (defvar save-place-forget-unreadable-files))
(eval-after-load 'saveplace
  (setq save-place-file (init-cache-path "saveplace")
        save-place-forget-unreadable-files t))
(add-hook 'init-first-input-hook #'save-place-mode)
;; Save Place in File:1 ends here

;; [[file:config.org::*Start Server][Start Server:1]]
(require 'server)
(unless (server-running-p)
  (server-start))
;; Start Server:1 ends here

;; [[file:config.org::*Emacs UI][Emacs UI:1]]
(defun init-font-exists-p (font-name)
  "Returns `t' if FONT-NAME is a valid font family."
  (member font-name (font-family-list)))

(defun init-find-font ()
  (cond ((init-font-exists-p "Fira Code")
	 (font-spec :family "Fira Code" :size 18))
	(t nil)))

(defun init-variable-pitch-font ()
  (cond ((init-font-exists-p "Fira Sans")
	 (font-spec :family "Fira Sans" :size 18))
	(t nil)))

(defvar init-font (init-find-font)
  "The default font to use.

Inspired by the way Doom Emacs handles `doom-font'.")

(defvar init-variable-pitch-font (init-variable-pitch-font))
(defvar init-serif-font nil)

(defun init-load-fonts ()
  "Loads `init-font'."
  (dolist (pair `((default . ,init-font)
		  (fixed-pitch . ,init-font)
		  (fixed-pitch-serif . ,init-serif-font)
		  (variable-pitch . ,init-variable-pitch-font)))
    (when-let* ((face (car pair))
		(font (cdr pair)))
	(set-face-attribute face nil
			  :width 'normal :weight 'normal
			  :slant 'normal :font font))))
(add-hook 'init-first-buffer-hook
	  #'init-load-fonts)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(blink-cursor-mode -1)
(setq x-stretch-cursor nil)

(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq frame-resize-pixelwise t)

(setq window-resize-pixelwise nil)

;; Doom says this is faster than calling the corresponding functions. See
;; core-ui.el
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(add-hook 'init-first-buffer-hook
	  #'window-divider-mode)

;; Avoid inconsistent GUIs
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

(setq split-width-threshold 160
      split-height-threshold nil)

(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(setq resize-mini-windows 'grow-only)
(advice-add #'yes-or-no-p :override #'y-or-n-p)

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Emacs UI:1 ends here

;; [[file:config.org::*Git][Git:1]]
(use-package magit
  :bind ("C-x g" . #'magit-status))
;; Git:1 ends here

;; [[file:config.org::*Lisp][Lisp:1]]
(defun init-lispy-on ()
  (lispy-mode 1))

(use-package lispy
  :hook ((emacs-lisp-mode . init-lispy-on)
	 (scheme-mode . init-lispy-on)
	 (clojure-mode . init-lispy-on)))
;; Lisp:1 ends here

;; [[file:config.org::*Clojure][Clojure:1]]
(use-package cider
  :hook ((cider-mode . eldoc-mode)))
;; Clojure:1 ends here

;; [[file:config.org::*Show Benchmarking][Show Benchmarking:1]]
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message "Initialization time %.2fs (+ %.2f system time)"
           init-time (- total-time init-time)))
;; Show Benchmarking:1 ends here

;; [[file:config.org::*Finalize][Finalize:1]]
(provide 'init)
;;; init.el ends here
;; Finalize:1 ends here
