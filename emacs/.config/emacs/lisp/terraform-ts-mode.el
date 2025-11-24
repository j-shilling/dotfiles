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

(require 'json)

(defvar-local terraform-ts-mode--flymake-proc nil)

(defun terraform-ts-mode--flymake-make-diagnostic (source diag)
  ""

  (let-alist diag
    (flymake-make-diagnostic
     .range.filename
     (cons .range.start.line .range.start.column)
     (cons .range.end.line .range.end.column)
     (cond
      ((string-equal .severity "error")
       :error)
      ((string-equal .severity "warning")
       :warning)
      (t :note))
     .detail)))

(defun terraform-ts-mode--flymake-sentinel (source)
  (lambda (proc _event)
    (with-current-buffer source
      (when (and (memq (process-status proc) '(exit signal))
                 (eq proc terraform-ts-mode--flymake-proc))
        (unwind-protect
            (progn
              (goto-char (point-min))
              (let* ((parsed (json-read))
                     (diags (alist-get 'diagnostics parsed)))
                (funcall report-fn
                         (mapcar #'terraform-ts-mode--flymake-make-diagnostic
                                 diags)))))
        (kill-buffer (process-buffer proc))))))

(defun terraform-ts-mode-flymake (report-fn &rest _more)
  "A flymake backend for terraform.
Calls REPORT-FN after using `terraform-command' to validate the current module."
  (when (process-live-p terraform-ts-mode--flymake-proc)
    (kill-process terraform-ts-mode--flymake-proc))

  (let ((source (current-buffer)))
    (setq terraform-ts-mode--flymake-proc
          (make-process
           :name "terraform-flymake" :noquery t :connection-type 'pipe
           :buffer (generate-new-buffer "*terraform-flymake*")
           :command `(,terraform-command "validate" "-json")
           :sentinel (terraform-ts-mode--flymake-sentinel source)))))

(defun terraform-ts-mode-setup-flymake-backend ()
  ""
  (add-hook 'flymake-diagnostic-functions 'terraform-ts-mode-flymake nil t))

;;; TODO:
;;; - face-lock-doc-face
(defvar terraform-ts-mode--treesit-font-lock-settings
  ;; todo
  '(:language hcl
              :override t
              :feature comments
              ((comment) @font-lock-comment-face)

              :language hcl
              :override t
              :feature contants
              ((literal_value) @font-lock-constant-face)

              :language hcl
              :override t
              :feature constants
              ([(string_lit) (quoted_template)] @font-lock-string-face)

              :language hcl
              :override t
              :feature brackets
              (["(" ")" "[" "]" "{" "}"
                (template_interpolation_start)
                (template_interpolation_end)]
               @font-lock-bracket-face)

              :language hcl
              :override t
              :feature delimiters
              (["." ".*" "," "[*]" "=>"] @font-lock-delimiter-face)

              :language hcl
              :override t
              :feature operators
              (["!"] @font-lock-negation-char-face)

              :language hcl
              :override t
              :feature operators
              (["\*" "/" "%" "\+" "-" ">" ">=" "<" "<=" "==" "!=" "&&" "||"] @font-lock-operator-face)

              :language hcl
              :override t
              :feature expressions
              ((variable_expr (identifier) @font-lock-variable-name-face)
               (get_attr (identifier) @font-lock-variable-name-face))

              :language hcl
              :feature builtin
              ((function_call (identifier) @font-lock-builtin-face))

              :language hcl
              :override t
              :feature blocks
              ((block (identifier) @font-lock-builtin-face
                      (string_lit) @font-lock-type-face
                      (string_lit) @font-lock-function-name-face))

              :language hcl
              :override t
              :feature blocks
              ((block (identifier) @font-lock-builtin-face
                      (string_lit) @font-lock-function-name-face))

              :language hcl
              :override t
              :feature blocks
              ((block (identifier) @font-lock-builtin-face))


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
    ;;
    ;; Navigation
    (setq-local treesit-defun-type-regexp "block")
    (setq-local treesit-defun-name-function #'terraform-ts-mode--treesit-defun-name)

    ;; Fontification.
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       terraform-ts-mode--treesit-font-lock-settings))
    (setq-local treesit-font-lock-feature-list '((comments)
                                                 (keywords attributes blocks strings numbers constants objects output modules workspaces vars)
                                                 (builtin brackets delimiters expressions operators interpolations conditionals)
                                                 ()))

    (treesit-major-mode-setup)))

(provide 'terraform-ts-mode)
;;; terraform-ts-mode.el ends here
