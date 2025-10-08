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
  (setq treesit-language-source-alist
        '((awk "https://github.com/Beaglefoot/tree-sitter-awk")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                      "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
          (markdown
           "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")

          (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")

          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master"
               "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
          (mermaid "https://github.com/monaqa/tree-sitter-mermaid")))
  (defun init-tree-sitter-install-all ()
    (interactive)
    (mapc (lambda (lang)
            (treesit-install-language-grammar lang (init-lib-state-file "tree-sitter")))
          (mapcar #'car treesit-language-source-alist))))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
