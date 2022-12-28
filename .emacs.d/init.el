;;; init.el --- Configure Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

;;;
;;; System Information
;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;;;
;;; Logging (copied from doom)
;;;

(defvar init-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `init-log' output.")

(defun init--log (text &rest args)
  (let ((inhibit-message (not init-file-debug))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat
            (lambda (x) (format "%s" x))
            (unless absolute?
              (when (bound-and-true-p init--current-module)
                  init--current-module))
            ":")
           args)))

(defmacro init-log (message &rest args)
  "Log a message in *Messages*.

Does not emit the message in the echo area. This is a macro instead of a
function to prevent the potentially expensive evaluation of its arguments when
debug mode is off. Return non-nil."
  (declare (debug t))
  `(unless init-inhibit-log (init--log ,message ,@args)))

;;;
;;; Custom Initialization Hooks
;;;

(defun init-unquote (exp)
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

  FORMS are evaluated once, when the hook is first invoked, then
  never again.

  HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function.

  This is taken from Doom Emacs."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (intern (format "init--transient-%d-h"
                            (put 'add-transient-hook! 'counter
                                 (1+ (or (get 'add-transient-hook! 'counter)
                                         0)))))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (init-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

;; Doom Style Init Hooks
(require 'nadvice)
(defun init-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once after any TRIGGER-HOOKS.

  This is a simplified version of `doom-run-hook-on' from Doom
  Emacs.

  HOOK-VAR is a quoted hook.
  TRIGGER-HOOKS is a list of quoted hooks."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            (when (and after-init-time
                       (not running?)
                       (or (daemonp)
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)
              (run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

(defvar init-switch-buffer-hook nil)
(defvar init-switch-window-hook nil)
(defvar init-switch-frame-hook nil)

(defun init-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'init-switch-buffer-hook)))
(defun init-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'init-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'init-switch-window-hook))))

(add-hook 'window-selection-change-functions #'init-run-switch-window-or-frame-hooks-h)
(add-hook 'window-buffer-change-functions #'init-run-switch-buffer-hooks-h)
(add-hook 'server-visit-hook #'init-run-switch-buffer-hooks-h)

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
  (init-run-hook-on 'init-first-buffer-hook '(find-file-hook init-switch-buffer-hook))
  (init-run-hook-on 'init-first-file-hook '(find-file-hook dired-initial-position-hook))
  (init-run-hook-on 'init-first-input-hook '(pre-command-hook)))

(unless init-inhibit-log
  (add-hook 'init-switch-frame-hook (lambda ()
                                      (init-log "Running init-switch-frame-hook")))
  (add-hook 'init-switch-buffer-hook (lambda ()
                                       (init-log "Running init-switch-buffer-hook")))
  (add-hook 'init-switch-window-hook (lambda ()
                                       (init-log "Running init-switch-window-hook")))
  (add-hook 'init-first-file-hook (lambda ()
                                    (init-log "Running init-first-file-hook")))
  (add-hook 'init-first-input-hook (lambda ()
                                     (init-log "Running init-first-input-hook")))
  (add-hook 'init-first-buffer-hook (lambda ()
                                      (init-log "Running init-first-buffer-hook"))))

;;;
;;; XDG Directories
;;;

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

;;;
;;; Package Management
;;;

;; Setup Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(require 'use-package)

;;;
;;; General Setup
;;;

(setq native-comp-deferred-compilation nil)

(setq user-full-name "Jake Shilling"
      user-email-address "shilling.jake@gmail.com")

(setq custom-file (init-cache-path "custom.el"))
(load custom-file t)

;;;
;;; Load Modules
;;;

(defvar init--current-module nil)

(defconst MODULES-DIR
  (expand-file-name "modules/" user-emacs-directory))

(when (file-directory-p MODULES-DIR)
  (let ((modules (directory-files MODULES-DIR t ".el$")))
    (dolist (module modules)
      (let ((init--curent-module module))
        (load-file module)))))

(provide 'init)
;;; init.el ends here
