;;; gptel-filesystem.el --- GPTel tools for working with files -*- lexical-binding: t -*-

;; Author: Jake Shilling

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

;; This is an attempt to port the filesystem MCP server to Emacs.

;;; Code:

(defun gptel-filesystem--read-text-file (path &optional head tail)
  "Read the complete contents of a `PATH' from the file system as text.
Handles various text encodings and provides detailed error messages if
the file cannot be read. Use this tool when you need to examine the
contents of a single file. Use the `HEAD' parameter to read only the
first N lines of a file, or the `TAIL' parameter to read only the last N
lines of a file. Operates on the file as text regardless of extension."
  (let ((content
         (with-temp-buffer
           (insert-file-contents path)
           (cond
            (head
             (forward-line head)
             (buffer-substring-no-properties (point-min) (point)))
            (tail
             (goto-char (point-max))
             (forward-line (- tail))
             (buffer-substring-no-properties (point) (point-max)))
            (t
             (buffer-substring-no-properties (point-min) (point-max)))))))
    `((content . (((type . "text")
                   (content . ,content))))
      (structuredContent . ((content . ,content))))))

(defun gptel-filesystem--read-media-file (path)
  "Read `PATH' as an image or audio file.
Returns the base64 encoded data and MIME type."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (base64-encode-region (point-min) (point-max))
    (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
           (mime-type (or (assoc-default (file-name-extension path t)
                                         '((".png" . "image/png")
                                           (".jpg" . "image/jpeg")
                                           (".jpeg" . "image/jpeg")
                                           (".gif" . "image/gif")
                                           (".webp" . "image/webp")
                                           (".bmp" . "image/bmp")
                                           (".svg" . "image/svg+xml")
                                           (".mp3" . "audio/mpeg")
                                           (".wav" . "audio/wav")
                                           (".ogg" . "audio/ogg")
                                           (".flac" . "audio/flac"))
                                         #'string=)
                          "application/octet-stream"))
           (type (cond
                  ((string-prefix-p "image" mime-type)
                   "image")
                  ((string-prefix-p "audio" mime-type)
                   "audio")
                  (t "blog"))))
      `((content . (((data . ,data)
                     (mimeType . ,mime-type)
                     (type . ,type))))
        (structuredContent . (content . (((data . ,data)
                                          (mimeType . ,mime-type)
                                          (type . ,type)))))))))

(defun gptel-filesystem--write-file (path content)
  "Write `CONTENT' to `PATH'.
Create a new file or completely overwrite an existing file with new
content. Use with caution as it will overwrite existing files without
warning. Handles text content with proper encoding."
  (write-region content nil path)
  (let ((text (format "Successfully wrote to %s" path)))
    `((content . (((type . "text")
                   (text . ,text))))
      (structuredContent . ((content . ,text))))))

(defun gptel-filesystem--create-directory (path)
  "Create a new directory at `PATH'."
  (make-directory path t)
  (let ((text (format "Successfully created directory %s" path)))
    `((content . (((type . "text")
                   (text . ,text))))
      (structuredContent . ((content . ,text))))))

(defun gptel-filesystem--list-directory (path)
  "List files in `PATH'.
Get a detailed listing of all files and directories in a specified path.
Results clearly distinguish between files and directories with [FILE]
and [DIR] prefixes. This tool is essential for understanding directory
structure and finding specific files within a directory. Only works
within allowed directories."
  (let ((formatted (string-join
                    (mapcar
                     (lambda (f)
                       (concat
                        (if (file-directory-p f)
                            "[DIR] "
                          "[FILE] ")
                        f))
                     (directory-files path t)) "\n")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((content . ,formatted))))))

(defun gptel-filesystem--move-file (source destination)
  "Move or rename `SOURCE' to `DESTINATION'.
Can move files between directories and rename them in a single
operation. If the destination exists, the operation will fail. Works
across different directories and can be used for simple renaming within
the same directory."
  (rename-file source destination)
  (let* ((text (format "Successfully moved %s to %s" source destination))
         (content-block `((type . "text")
                          (text . ,text))))
    `((content . ,(list content-block))
      (structuredContent . ((content . ,(list content-block)))))))

(defun gptel-filesystem--get-file-info (path)
  "Retrieve detailed metadata about a `PATH'.
Returns comprehensive information including size, creation time, last
modified time, permissions, and type. This tool is perfect for
understanding file characteristics without reading the actual content.
Only works within allowed directories."
  (let* ((attrs (file-attributes path))
         (text (format
                "size: %d
accessed: %d
modified: %d
isDirectory: %s
isFile: %s
permissions: %s"
                (file-attribute-size attrs)
                (time-convert (file-attribute-access-time attrs) 'integer)
                (time-convert (file-attribute-modification-time attrs) 'integer)
                (if (file-directory-p path)
                    "true"
                  "false")
                (if (file-regular-p path)
                    "true"
                  "false")
                (file-attribute-modes attrs))))
    `((content . (((type . "text")
                   (text . ,text))))
      (structuredContent . ((content . ,text))))))

(gptel-make-tool
 :name "read_text_file"
 :function #'gptel-filesystem--read-text-file
 :description
 "Read the complete contents of a file from the file system as text.
Handles various text encodings and provides detailed error messages if
the file cannot be read. Use this tool when you need to examine the
contents of a single file. Use the 'head' parameter to read only the
first N lines of a file, or the 'tail' parameter to read only the last N
lines of a file. Operates on the file as text regardless of extension."
 :category "filesystem"
 :args (list (list :name "path"
                   :type 'string
                   :description "Path to the file to read")
             (list :name "head"
                   :type 'number
                   :description "If provided, returns only the first N lines of the file"
                   :optional t)
             (list :name "tail"
                   :type 'number
                   :description "If provided, returns only the last N lines of the file"
                   :optional t)))

(gptel-make-tool
 :name "read_media_file"
 :function #'gptel-filesystem--read-media-file
 :description
 "Read an image or audio file. Returns the base64 encoded data and MIME
type."
 :category "filesystem"
 :args (list (list :name "path"
                   :type 'string
                   :description "Path to the media file to read")))

(gptel-make-tool
 :name "write_file"
 :function #'gptel-filesystem--write-file
 :description
 "Create a new file or completely overwrite an existing file with new
content. Use with caution as it will overwrite existing files without
warning. Handles text content with proper encoding."
 :category "filesystem"
 :confirm t
 :args (list (list :name "path"
                   :type 'string
                   :description "Path where the file should be written")
             (list :name "content"
                   :type 'string
                   :description "Content to write to the file")))

;; (gptel-make-tool
;;  :name "edit_file"
;;  :function #'gptel-filesystem--edit-file
;;  :description "Make line-based edits to a text file. Each edit replaces exact line sequences with new content. Returns a git-style diff showing the changes made. Only works within allowed directories."
;;  :category "filesystem"
;;  :confirm t
;;  :args (list '(:name "path"
;;                      :type string
;;                      :description "Path to the file to edit")
;;              '(:name "edits"
;;                      :type array
;;                      :items (:type object
;;                                    :properties (:oldText (:type string
;;                                                                 :description "Text to search for - must match exactly")
;;                                                          :newText (:type string
;;                                                                          :description "Text to replace with")))
;;                      :description "Array of edit operations to perform")
;;              '(:name "dryRun"
;;                      :type boolean
;;                      :description "Preview changes using git-style diff format"
;;                      :optional t)))

(gptel-make-tool
 :name "create_directory"
 :function #'gptel-filesystem--create-directory
 :description
 "Create a new directory or ensure a directory exists. Can create multiple
nested directories in one operation. If the directory already exists,
this operation will succeed silently. Perfect for setting up directory
structures for projects or ensuring required paths exist."
 :category "filesystem"
 :args (list (list :name "path"
                   :type 'string
                   :description "Path of the directory to create")))

(gptel-make-tool
 :name "list_directory"
 :function #'gptel-filesystem--list-directory
 :description
 "Get a detailed listing of all files and directories in a specified path.
Results clearly distinguish between files and directories with [FILE]
and [DIR] prefixes. This tool is essential for understanding directory
structure and finding specific files within a directory. Only works
within allowed directories."
 :category "filesystem"
 :args (list (list :name "path"
                   :type 'string
                   :description "Path to the directory to list")))

;; (gptel-make-tool
;;  :name "list_directory_with_sizes"
;;  :function #'gptel-filesystem--list-directory-with-sizes
;;  :description
;;  "Get a detailed listing of all files and directories in a specified path,
;; including sizes. Results clearly distinguish between files and
;; directories with [FILE] and [DIR] prefixes. This tool is useful for
;; understanding directory structure and finding specific files within a
;; directory. Only works within allowed directories."
;;  :category "filesystem"
;;  :args (list (list :name "path"
;;                    :type 'string
;;                    :description "Path to the directory to list")
;;              (list :name "sortBy"
;;                    :type 'string
;;                    :enum ["name" "size"]
;;                    :description "Sort entries by name or size"
;;                    :optional t)))

;; (gptel-make-tool
;;  :name "directory_tree"
;;  :function #'gptel-filesystem--directory-tree
;;  :description "Get a recursive tree view of files and directories as a JSON structure. Each entry includes 'name', 'type' (file/directory), and 'children' for directories. Files have no children array, while directories always have a children array (which may be empty). The output is formatted with 2-space indentation for readability. Only works within allowed directories."
;;  :category "filesystem"
;;  :args (list '(:name "path"
;;                      :type string
;;                      :description "Root path for the directory tree")
;;              '(:name "excludePatterns"
;;                      :type array
;;                      :items (:type string)
;;                      :description "Glob patterns for files/directories to exclude"
;;                      :optional t)))

(gptel-make-tool
 :name "move_file"
 :function #'gptel-filesystem--move-file
 :description
 "Move or rename files and directories. Can move files between directories
and rename them in a single operation. If the destination exists, the
operation will fail. Works across different directories and can be used
for simple renaming within the same directory. Both source and
destination must be within allowed directories."
 :category "filesystem"
 :confirm t
 :args (list (list :name "source"
                   :type 'string
                   :description "Source path of the file or directory to move")
             (list :name "destination"
                   :type 'string
                   :description "Destination path where the file or directory should be moved")))

;; (gptel-make-tool
;;  :name "search_files"
;;  :function #'gptel-filesystem--search-files
;;  :description "Recursively search for files and directories matching a pattern. The patterns should be glob-style patterns that match paths relative to the working directory. Use pattern like '*.ext' to match files in current directory, and '**/*.ext' to match files in all subdirectories. Returns full paths to all matching items. Great for finding files when you don't know their exact location. Only searches within allowed directories."
;;  :category "filesystem"
;;  :args (list '(:name "path"
;;                      :type string
;;                      :description "Starting path for the search")
;;              '(:name "pattern"
;;                      :type string
;;                      :description "Glob-style pattern to match files")
;;              '(:name "excludePatterns"
;;                      :type array
;;                      :items (:type string)
;;                      :description "Glob patterns for files/directories to exclude from search"
;;                      :optional t)))

(gptel-make-tool
 :name "get_file_info"
 :function #'gptel-filesystem--get-file-info
 :description
 "Retrieve detailed metadata about a file or directory. Returns
comprehensive information including size, creation time, last modified
time, permissions, and type. This tool is perfect for understanding file
characteristics without reading the actual content. Only works within
allowed directories."
 :category "filesystem"
 :args (list (list :name "path"
                   :type 'string
                   :description "Path to the file or directory")))

(provide 'gptel-filesystem)
;;; gptel-filesystem.el ends here
