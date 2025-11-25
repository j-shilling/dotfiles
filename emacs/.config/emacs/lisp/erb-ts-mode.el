;;; erb-ts-mode.el --- ERB editing by tree sitter -*- lexical-binding: t -*-

;; Author: Jake Shilling
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

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
(require 'treesit)
(require 'html-ts-mode)
(require 'ruby-ts-mode)

(eval-when-compile
  (require 'rx))

(defvar erb-ts-mode--font-lock-settings
  (nconc (ruby-ts--font-lock-settings 'ruby)
         html-ts-mode--font-lock-settings))

(defun erb-ts-mode--language-at-point (pos)
  "Retrun the language at POS."
  (let ((node (treesit-node-at pos 'embedded-template)))
    (cond
     ((equal (treesit-node-type node) "content")
      'html)
     ((equal (treesit-node-type node) "code")
      'ruby)
     (t 'embedded-template))))

(defun erb-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-language node)
    ('html (html-ts-mode--defun-name (node)))
    (_ nil)))

;;;###autoload
(define-derived-mode erb-ts-mode html-ts-mode
  "ERB[TS]"
  "Major mode for editing ERB files."
  (unless (and (treesit-ready-p 'embedded-template)
               (treesit-ready-p 'html)
               (treesit-ready-p 'ruby))
    (error "Tree-sitter for ERB isn't available"))
  (setq-local treesit-primary-parser
              (treesit-parser-create 'embedded-template))

  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'html
               :host 'embedded-template
               '((content) @capture)
               :embed 'ruby
               :host 'embedded-template
               '(((directive "<%" (code) @capture "%>"))
                 ((output_directive "<%=" (code) @capture "%>")))))
  (setq-local treesit-language-at-point-function
              #'erb-ts-mode--language-at-point)

  ;; Navigation
  (setq-local treesit-defun-type-regexp (rx (or "content"
                                                "directive"
                                                "output_directive"
                                                (regex ruby-ts--method-regex))))

  (setq-local treesit-defun-name-function #'erb-ts-mode--defun-name)

  (setq-local treesit-thing-settings
              `((embedded-template
                 (sexp ,(rx (or "directive"
                                "output_directive"))))
                (html
                 (sexp ,(rx (or "element"
                                "text"
                                "attribute"
                                "value")))
                 (sentence "tag")
                 (text ,(rx (or "comment" "text"))))
                (ruby
                 (sexp ,(cons (rx
                               bol
                               (or
                                "class"
                                "singleton_class"
                                "module"
                                "method"
                                "singleton_method"
                                "array"
                                "hash"
                                "parenthesized_statements"
                                "method_parameters"
                                "array_pattern"
                                "hash_pattern"
                                "if"
                                "else"
                                "then"
                                "unless"
                                "case"
                                "case_match"
                                "when"
                                "while"
                                "until"
                                "for"
                                "block"
                                "do_block"
                                "begin"
                                "integer"
                                "identifier"
                                "self"
                                "super"
                                "constant"
                                "simple_symbol"
                                "hash_key_symbol"
                                "symbol_array"
                                "string"
                                "string_array"
                                "heredoc_body"
                                "regex"
                                "argument_list"
                                "interpolation"
                                "instance_variable"
                                "global_variable"
                                )
                               eol)
                              #'ruby-ts--sexp-p))
                 (text ,(lambda (node)
                          (or (member (treesit-node-type node)
                                      '("comment" "string_content" "heredoc_content"))
                              ;; for C-M-f in hash[:key] and hash['key']
                              (and (member (treesit-node-text node)
                                           '("[" "]"))
                                   (equal (treesit-node-type
                                           (treesit-node-parent node))
                                          "element_reference"))
                              ;; for C-M-f in "abc #{ghi} def"
                              (and (member (treesit-node-text node)
                                           '("#{" "}"))
                                   (equal (treesit-node-type
                                           (treesit-node-parent node))
                                          "interpolation"))))))))

  ;; Font Lock
  (setq-local treesit-font-lock-settings erb-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment keyword definition method-definition parameter-definition)
                ( property regexp string type)
                ( builtin-variable builtin-constant builtin-function
                  delimiter escape-sequence
                  constant global instance
                  interpolation literal symbol assignment)
                ( bracket error function operator punctuation)))

  ;; Outline minor mode
  (setq-local treesit-outline-predicate
              (rx bos (or "singleton_method"
                          "method"
                          "alias"
                          "class"
                          "module"
                          "element")
                  eos))

  (treesit-major-mode-setup))

(derived-mode-add-parents 'erb-ts-mode '(html-ts-mode))

(provide 'erb-ts-mode)
;;; erb-ts-mode.el ends here
