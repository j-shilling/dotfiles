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

(gptel-make-tool
 :name "create_buffer"
 :category "buffers"
 :description
 "Create a new buffer using `NAME'. If there is already a buffer called
`NAME', then append a suffix to `NAME' so that it becomes unique.
 Return the actual name of the resulting buffer."
 :args
 '((:name "name"
    :type string
    :description
    "The name used to create the buffer. If a buffer already
 exists with the same name, then a suffix will be addeed to make it
 unique."))
 :function
 (lambda (name)
   (buffer-name
    (generate-new-buffer name))))

(gptel-make-tool
 :name "list_buffers"
 :category "buffers"
 :description
 "Return a list of currently available buffers."
 :function
 (lambda ()
   (mapcar #'buffer-name (buffer-list))))

(gptel-make-tool
 :name "list_visible_buffers"
 :category "buffers"
 :description
 "Return a list of buffers that are currently displayed to the user."
 :function
 (lambda ()
   (let* ((ws (window-list))
          (bs (mapcar #'window-buffer ws)))
     (mapcar #'buffer-name bs))))

(gptel-make-tool
 :name "read_buffer"
 :category "buffers"
 :description
 "Return the text currently stored in `BUFFER'."
 :args
 '((:name "buffer"
    :type string
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (buffer)
   (let ((b (get-buffer buffer)))
     (unless (buffer-live-p b)
       (error "Error: buffer %s is not live." buffer))
     (with-current-buffer b
       (buffer-substring-no-properties (point-min) (point-max))))))

(gptel-make-tool
 :name "pop_to_buffer"
 :category "buffers"
 :description
 "Display buffer identified by `NAME' to the user."
 :args
 '((:name "buffer"
    :type string
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (buffer)
   (let ((b (get-buffer buffer)))
     (unless (buffer-live-p b)
       (error "Error: buffer %s is not live." buffer))
     (pop-to-buffer b))))

(gptel-make-tool
 :name "insert_into_buffer"
 :category "buffers"
 :description
 "Insert `TEXT' into `BUFFER'. If `POINT' is specified, then then the
 text will be inserted at that location. Otherwise, the `TEXT' is
 inserted at the end of the buffer.'"
 :args
 '((:name "buffer"
    :type string
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools.")
   (:name "text"
    :type string
    :description
    "The text to be inserted.")
   (:name "loc"
    :type integer
    :optional t
    :description
    "An integer no less than 1 which services as an index between two
 characters in the buffer's string contents. Inserting text at index 1
 would put the text at the beginning of the buffer. If no value is
 provided, this defaults to the end."))
 :function
 (lambda (buffer text &optional loc)
   (let ((b (get-buffer b)))
     (unless (buffer-live-p b)
       (error "Error: buffer %s is not live." buffer))
     (save-excursion
       (with-current-buffer b
         (goto-char (or loc (point-max)))
         (insert text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "create_file_buffer"
 :category "buffers"
 :description
 "Read file FILENAME into a buffer and return the buffer's name. If a
buffer exists visiting FILENAME, return that one."
 :args
 '((:name "filename"
    :type string
    :description
    "Name of the file to read"))
 :function
 (lambda (filename)
   (find-file-noselect filename t)))

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
;;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "get_project_root"
 :category "project"
 :description
 "Return the root directory of the current project. If `BUFFER' is
provided, then use the buffer identified by `BUFFER' to determine what
 the current project is. Otherwise, use the current buffer."
 :args
 '((:name "buffer"
    :type string
    :optional t
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (&optional buffer)
   (let ((dir (if buffer
                  (let ((b (get-buffer buffer)))
                    (unless (buffer-live-p b)
                      (error "error: buffer %s is not live." buffer))
                    default-directory)
                default-directory)))
     (project-root (project-current nil dir)))))

(gptel-make-tool
 :name "get_project_name"
 :category "project"
 :description
 "Return the name of the current project. If `BUFFER' is
provided, then use the buffer identified by `BUFFER' to determine what
 the current project is. Otherwise, use the current buffer."
 :args
 '((:name "buffer"
    :type string
    :optional t
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (&optional buffer)
   (let ((dir (if buffer
                  (let ((b (get-buffer buffer)))
                    (unless (buffer-live-p b)
                      (error "error: buffer %s is not live." buffer))
                    default-directory)
                default-directory)))
     (project-name (project-current nil dir)))))

(gptel-make-tool
 :name "list_project_buffers"
 :category "project"
 :description
 "Return a list of buffers associated with the current project. If
 `BUFFER' is provided, then use the buffer identified by `BUFFER' to
 determine what the current project is. Otherwise, use the current
 buffer."
 :args
 '((:name "buffer"
    :type string
    :optional t
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (&optional buffer)
   (let ((dir (if buffer
                  (let ((b (get-buffer buffer)))
                    (unless (buffer-live-p b)
                      (error "error: buffer %s is not live." buffer))
                    default-directory)
                default-directory)))
     (project-buffers (project-current nil dir)))))

(gptel-make-tool
 :name "list_project_files"
 :category "project"
 :description
 "Return a list of files associated with the current project. If
 `BUFFER' is provided, then use the buffer identified by `BUFFER' to
 determine what the current project is. Otherwise, use the current
 buffer."
 :args
 '((:name "buffer"
    :type string
    :optional t
    :description
    "A name identifying which buffer to read. This can be the return
 value of `create_buffer' or one of the names returned by
 `list_buffers', `list_visible_buffers', or similar tools."))
 :function
 (lambda (&optional buffer)
   (let ((dir (if buffer
                  (let ((b (get-buffer buffer)))
                    (unless (buffer-live-p b)
                      (error "error: buffer %s is not live." buffer))
                    default-directory)
                default-directory)))
     (project-files (project-current nil dir)))))

(provide 'init-ai-tools)
;;; init-ai-tools.el ends here
