;;; init-lib.el --- Misc functions used throughout init files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jake Shilling

;; Author: Jake Shilling <shilling.jake@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(defun init--parts-to-path (&rest args)
  "Return the result of joining `ARGS' into a single path."
  (require 'seq)
  (seq-reduce (lambda (acc part)
                (expand-file-name part acc))
              args
              default-directory))

(defun init-lib-state-file (&rest args)
  "Return `ARGS' as a path relative to the state directory."
  (require 'xdg)
  (declare-function xdg-state-home 'xdg)
  (apply #'init--parts-to-path (xdg-state-home) "emacs" emacs-version args))

(defun init-lib-cache-file (&rest args)
  "Return `ARGS' as a path relative to the cache directory."
  (require 'xdg)
  (declare-function xdg-cache-home 'xdg)
  (apply #'init--parts-to-path (xdg-cache-home) "emacs" emacs-version args))

(defun init-lib-config-file (&rest args)
  "Return `ARGS' as a path relative to `user-emacs-directory'."
  (apply #'init--parts-to-path user-emacs-directory args))

(defun init-lib-mac-p ()
  "Return non-nil if this is running on a mac."
  (eq system-type 'darwin))

(defun init-lib-linux-p ()
  "Return non-nil if this is running in a linux environment."
  (eq system-type 'gnu/linux))

(defun init-lib-windows-p ()
  "Return non-nil if this is running in a windows environment."
  (memq system-type '(cygwin windows-nt ms-dos)))

(defun init-lib-wsl-p ()
  "Return non-nil if this is running in a WSL environment."
  (and (init-lib-linux-p)
       (string-match-p "Microsoft" (shell-command-to-string "uname -a"))))

(provide 'init-lib)
;;; init-lib.el ends here
