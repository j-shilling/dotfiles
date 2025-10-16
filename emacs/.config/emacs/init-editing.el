;;; init-editing.el --- Configure how to interact with text  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jake Shilling

;; Author: Jake Shilling <shilling.jake@gmail.com>
;; Keywords:

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
(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

(use-package autorevert
  :hook
  ((after-init-hook . auto-revert-mode)))

(use-package multiple-cursors
  :if (package-installed-p 'multiple-cursors)
  :commands mc/sort-regions
  :custom
  (mc/list-file (init-lib-cache-file ".mc-lists.el"))
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package avy
  :if (package-installed-p 'avy)
  :bind
  (("C-;" . 'avy-goto-char)
   ("C-'" . 'avy-goto-char-2)
   ("M-g f" . 'avy-goto-line)
   ("M-g w" . 'avy-goto-word-1)
   ("M-g e" . 'avy-goto-word-0)
   ("C-c C-j" . 'avy-resume)))

(use-package ace-window
  :if (package-installed-p 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("M-o" . 'ace-window)))

(use-package delsel
  :hook
  ((after-init-hook . delete-selection-mode)))

(provide 'init-editing)
;;; init-editing.el ends here
