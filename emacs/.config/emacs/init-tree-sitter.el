;;; init-tree-sitter.el --- Configuration related to tree-sitter  -*- lexical-binding: t; -*-

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

(require 'init-lib (expand-file-name "init-lib.el" user-emacs-directory))

(use-package treesit
    :init
  (add-to-list 'treesit-extra-load-path
               (init-lib-state-file "tree-sitter"))
  ;; TODO:
  ;; - https://github.com/tree-sitter/tree-sitter-c
  ;; - https://github.com/tree-sitter/tree-sitter-embedded-template
  ;; - https://github.com/tree-sitter/tree-sitter-haskell
  ;; - https://github.com/tree-sitter/tree-sitter-jsdoc
  ;; - https://github.com/tree-sitter-grammars/tree-sitter-csv
  ;; - https://github.com/tree-sitter-grammars/tree-sitter-gitattributes
  ;; - https://github.com/tree-sitter-grammars/tree-sitter-hcl
  ;; - https://github.com/tree-sitter-grammars/tree-sitter-scss
  ;; - https://github.com/tree-sitter-grammars/tree-sitter-xml
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
          (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
          (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
           "v0.23.1")
          (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make" "v1.1.1")
          (markdown
           "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
           "v0.4.1"
           "tree-sitter-markdown/src")
          (markdown-inline
           "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
           "v0.4.1"
           "tree-sitter-markdown-inline/src")
          (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2"
           "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
           "v0.23.2" "typescript/src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.2")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl" "v1.2.0")))
  (defun init-tree-sitter-install-all ()
    (interactive)
    (mapc (lambda (lang)
            (treesit-install-language-grammar lang (init-lib-state-file "tree-sitter")))
          (mapcar #'car treesit-language-source-alist))))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
