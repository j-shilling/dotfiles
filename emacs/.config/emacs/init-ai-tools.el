;;; init-ai-tools.el --- Tools used by GPTel         -*- lexical-binding: t; -*-

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

(require 'gptel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gptel-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gptel-filesystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diffs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "create_patch_buffer"
 :category "diffs"
 :description
 "Create a new buffer using `NAME' that displays a change to the user. If
there is already a buffer called `NAME', then append a suffix to `NAME'
 so that it becomes unique. Return the actual name of the resulting
 buffer."
 :args
 '((:name "name"
          :type string
          :description
          "The name used to create the buffer. If a buffer already
 exists with the same name, then a suffix will be addeed to make it
 unique.")
   (:name "text"
          :type string
          :description
          "A string containing the patch in standard diff format"))
 :function
 (lambda (name text)
   (let ((b (generate-new-buffer name)))
     (with-current-buffer b
       (insert patch)
       (diff-mode))
     (pop-to-buffer b)
     (buffer-name b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "get_buffer_diagnostics"
 :category "flymake"
 :description
 "Return a JSON array describing errors within a buffer"
 :args
 '((:name "buffer"
          :type string
          :description
          "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools.")
   (:name "beg"
          :type integer
          :optional t
          :description "If provided, searches for diagnostics after this point")
   (:name "beg"
          :type integer
          :optional t
          :description "If provided, searches for diagnostics before this point"))
 :function
 (lambda (buffer &optional beg end)
   (unless (buffer-live-p (get-buffer buffer))
     (error "error: buffer %s is not live." buffer))
   (with-current-buffer buffer
     (when-let ((diags (flymake-diagnostics beg end)))
       (json-encode
        (mapcar #'flymake-diagnostic-data
                diags))))))

(gptel-make-tool
 :name "get_buffer_diagnostics_by_line_number"
 :category "flymake"
 :description
 "Return a JSON array describing errors at a line"
 :args
 '((:name "buffer"
          :type string
          :description
          "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools.")
   (:name "line"
          :type integer
          :description "The line to examine"))
 :function
 (lambda (buffer line)
   (unless (buffer-live-p (get-buffer buffer))
     (error "error: buffer %s is not live." buffer))
   (with-current-buffer buffer
     (let ((region (flymake-diag-region buffer line)))
       (when-let ((diags (flymake-diagnostics (car region) (cdr region))))
         (json-encode
          (mapcar #'flymake-diagnostic-data
                  diags)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gptel-xref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gptel-project)

(provide 'init-ai-tools)
;;; init-ai-tools.el ends here
