;;; terraform-ts-mode.el --- Mode to edit terraform files powered by treesit -*- lexical-binding: t -*-

;; Author: Jake Shilling
;; Package-Requires:

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

;;; Code:

(defvar terraform-ts-mode--treesit-font-lock-settings
  ;; todo
  (treesit-font-lock-rules
   ))

(defun terraform-ts-mode--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "block")
    (treesit-node-text
     (treesit-node-child
      (treesit-node-child node
                          (pcase (treesit-node-text (treesit-node-child node 0) t)
                            ("resource" 2)
                            ("data" 2)
                            ("variable" 1)
                            ("provider" 1)
                            ("module" 1)
                            ("output" 1)))
      1)
     t)))

;;;###autoload
(define-derived-mode terraform-ts-mode terraform-mode
  "Terraform[TS]"
  "Major ode for editing terraform files."
  (when (treesit-ready-p 'hcl)
    (setq-local treesit-primary-parser (treesit-parser-create 'hcl))

    ;; Navigation
    (setq-local treesit-defun-type-regexp "block")
    (setq-local treesit-defun-name-function #'terraform-ts-mode--treesit-defun-name)

    ;; Fontification.
    (setq-local treesit-font-lock-settings terraform-ts-mode--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment document definition)
                  ( keyword string)
                  ( assignment constant escape-sequence jsx number
                   pattern string-interpolation)
                  ( bracket delimiter function operator property)))

    (treesit-major-mode-setup)))

(provide 'terraform-ts-mode)
;;; terraform-ts-mode.el ends here
