;;; markdown-ts-mode.el --- Markdown supported by tree sitter -*- lexical-binding: t -*-

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

(require 'markdown-mode)

(define-derived-mode markdown-ts-mode markdown-mode "Markdown[TS]"
                     "Major mode for editing markdown files."
                     (when (and (treesit-ready-p 'markdown-inline t)
                                (treesit-ready-p 'markdown t))
                       (treesit-major-mode-setup)))

(provide 'markdown-ts-mode)
;;; markdown-ts-mode.el ends here
