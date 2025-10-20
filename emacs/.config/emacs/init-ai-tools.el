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
 :category "files"
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

(gptel-make-tool
 :name "list_directory_files"
 :category "files"
 :description
 "This function returns a list of the names of the files in the
directory DIRECTORY.  By default, the list is in alphabetical
order.

If FULL-NAME is `true', the function returns the files’ absolute
file names.  Otherwise, it returns the names relative to the
specified directory.

If MATCH-REGEXP is provided, this function returns only those file
names whose non-directory part contain a match for that regular
expression—the other file names are excluded from the list.  On
case-insensitive filesystems, the regular expression matching is
case-insensitive.

If NOSORT is `true', this does not sort the list, so you get the file
names in no particular order. Use this if you want the utmost possible
speed and don’t care what order the files are processed in. If the order
of processing is visible to the user, then the user will probably be
happier if you do sort the names.

If COUNT is provided, the function will return names of first
COUNT number of files, or names of all files, whichever occurs
first.  COUNT has to be an integer greater than zero."
 :args
 '((:name "directory"
    :type string
    :description
    "Name of the directory to examine")
   (:name "fullName"
    :type boolean
    :optional t
    :description
    "When, `true', this function returns the files' absolute file names.
Defaults to `false'.")
   (:name "matchRegexp"
    :type string
    :optional t
    :description
    "When provided, this should be a regular expression. Only files use
non-directory part contain a match will be returned.")
   (:name "noSort"
    :type boolean
    :optional t
    :description
    "When `true', results are returned in no particular order. Use this when
you want the utmost possible speed. Defaults to `false'.")
   (:name "count"
    :type integer
    :optional t
    :description
    "When provided, this function will return limit the number of files
returned to this number. It must be an integer greater than 0."))
 :function
 (lambda (directory &optional full-name match-regexp nosort count)
   (directory-files directory full-name match-regexp nosort count)))

(gptel-make-tool
 :name "list_directory_files_recursively"
 :category "files"
 :description
 "Return all files under DIRECTORY whose names match REGEXP.  This
function searches the specified DIRECTORY and its sub-directories,
recursively, for files whose basenames (i.e., without the leading
directories) match the specified REGEXP, and returns a list of the
absolute file names of the matching files. The file names are returned
in depth-first order, meaning that files in some sub-directory are
returned before the files in its parent directory. In addition, matching
files found in each subdirectory are sorted alphabetically by their
basenames. By default, directories whose names match REGEXP are omitted
from the list, but if the optional argument INCLUDE-DIRECTORIES is
`true', they are included.

Symbolic links to subdirectories are not followed by default, but
if FOLLOW-SYMLINKS is `true', they are followed."
 :args
 '((:name "directory"
    :type string
    :description
    "Name of the directory to examine")
   (:name "regexp"
    :type string
    :description
    "Should be a regular expression. Only files use non-directory part
contain a match will be returned.")
   (:name "includeDirectories"
    :type boolean
    :optional t
    :description
    "When `true', include directories whose name matches `REGEX'. Defaults to
`false'.")
   (:name "followSymlinks"
    :type boolean
    :optional t
    :description
    "When `true', this function will follow symlinks. Defaults to `false'."))
 :function
 (lambda (directory regexp &optional include-directories follow-symlinks)
   (directory-files-recursively directory regexp include-directories nil follow-symlinks)))

(gptel-make-tool
 :name "find_dominating_file"
 :category "files"
 :description
 "Starting at FILE, go up the directory tree hierarchy looking for
the first directory where NAME, a string, exists, and return that
directory.  If FILE is a file, its directory will serve as the
starting point for the search; otherwise FILE should be a directory
from which to start.  The function looks in the starting directory,
then in its parent, then in its parent’s parent, etc., until it
either finds a directory with NAME or reaches the root directory of
the filesystem without finding NAME – in the latter case the
function returns `null'."
 :args
 '((:name "file"
    :type string
    :description
    "Name of the file in the directory tree hierarchy to start at.")
   (:name "name"
    :type string
    :description
    "The name of the file to search for."))
 :function
 (lambda (file name)
   (locate-dominating-file file name)))

(gptel-make-tool
 :name "expand_file_wildcards"
 :category "files"
 :description
 "This function expands the wildcard pattern PATTERN, returning a
list of file names that match it.

PATTERN is, by default, a “glob”/wildcard string, e.g.,
‘\"/tmp/*.png\"’ or ‘\"/*/*/foo.png\"’, but can also be a regular
expression if the optional REGEXP parameter is non-‘nil’.  In any
case, the matches are applied per sub-directory, so a match can’t
span a parent/sub directory.

If PATTERN is written as an absolute file name, the values are
absolute also.

If PATTERN is written as a relative file name, it is interpreted
relative to the current default directory.  The file names returned
are normally also relative to the current default directory.
However, if FULL is non-‘nil’, they are absolute."
 :args
 '((:name "pattern"
    :type string
    :description
    "The pattern with wildcards to try to expand.")
   (:name "full"
    :type boolean
    :optional t
    :description
    "When `true', the results are absolute file names, otherwise they are
relative to the current directory. Defaults to `false'.")
   (:name "regexp"
    :type boolean
    :optional t
    :description
    "When `true', then `PATTERN' is treated as a regular expression instead
of a wildcard pattern. Defaults to `false'."))
 :function
 (lambda (pattern &optional full regexp)
   (file-expand-wildcards pattern full regexp)))

(gptel-make-tool
 :name "make_directory"
 :category "files"
 :description
 "Create a directory named DIRNAME. If PARENTS is `true', create the
parent directories first if they don't already exist (similar to
'mkdir -p'). Returns `true' if DIRNAME already exists as a directory
and PARENTS is `true', otherwise returns `false' on successful creation."
 :args
 '((:name "dirname"
    :type string
    :description
    "Name of the directory to create. Can be an absolute or relative path.")
   (:name "parents"
    :type boolean
    :optional t
    :description
    "When `true', create parent directories if they don't exist. Defaults
to `false'."))
 :function
 (lambda (dirname &optional parents)
   (make-directory dirname parents)))

(gptel-make-tool
 :name "make_empty_file"
 :category "files"
 :description
 "Create an empty file named FILENAME. If PARENTS is `true', create the
parent directories first if they don't already exist. Signals an error
if FILENAME already exists."
 :args
 '((:name "filename"
    :type string
    :description
    "Name of the empty file to create. Can be an absolute or relative path.")
   (:name "parents"
    :type boolean
    :optional t
    :description
    "When `true', create parent directories if they don't exist. Defaults
to `false'."))
 :function
 (lambda (filename &optional parents)
   (make-empty-file filename parents)))

(gptel-make-tool
 :name "copy_directory"
 :category "files"
 :description
 "Copy the directory named DIRNAME to NEWNAME. If NEWNAME is a directory
name, DIRNAME will be copied to a subdirectory there. Always sets the
file modes of copied files to match the original files."
 :args
 '((:name "dirname"
    :type string
    :description
    "Name of the source directory to copy.")
   (:name "newname"
    :type string
    :description
    "Destination path. If this is a directory, DIRNAME will be copied as
a subdirectory within it.")
   (:name "keepTime"
    :type boolean
    :optional t
    :description
    "When `true', preserve the modification time of copied files. Defaults
to `false'.")
   (:name "parents"
    :type boolean
    :optional t
    :description
    "When `true', create parent directories if they don't exist. Defaults
to `false'.")
   (:name "copyContents"
    :type boolean
    :optional t
    :description
    "When `true', if NEWNAME is a directory, copy the contents of DIRNAME
directly into it instead of copying DIRNAME as a subdirectory. Defaults
to `false'."))
 :function
 (lambda (dirname newname &optional keep-time parents copy-contents)
   (copy-directory dirname newname keep-time parents copy-contents)))

(gptel-make-tool
 :name "delete_directory"
 :category "files"
 :description
 "Delete the directory named DIRNAME. If RECURSIVE is `false' and the
directory contains files, signals an error. Only follows symbolic links
at the level of parent directories."
 :args
 '((:name "dirname"
    :type string
    :description
    "Name of the directory to delete.")
   (:name "recursive"
    :type boolean
    :optional t
    :description
    "When `true', delete the directory and all its contents recursively.
When `false', signals an error if directory contains files. Defaults
to `false'.")
   (:name "trash"
    :type boolean
    :optional t
    :description
    "When `true' and `delete-by-moving-to-trash' is non-nil, move the
directory to system Trash instead of deleting it. Defaults to `false'."))
 :function
 (lambda (dirname &optional recursive trash)
   (delete-directory dirname recursive trash)))

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
