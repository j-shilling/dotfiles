;;; gptel-project.el --- GPTel tools for working with projects -*- lexical-binding: t -*-

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

;; This package provides GPTel tools for working with Emacs projects.
;; Projects are typically defined by version control systems (like Git),
;; or by project.el configuration files. These tools help navigate and
;; understand project structure.

;;; Code:

(require 'gptel)
(require 'project)

(defun gptel-project--get-project (buffer)
  "Get the project associated with BUFFER.
Returns the project object or signals an error if no project is found.
BUFFER can be nil to use the current buffer."
  (let* ((dir (if buffer
                  (let ((b (get-buffer buffer)))
                    (unless (buffer-live-p b)
                      (error "Buffer %s is not live" buffer))
                    (with-current-buffer b
                      default-directory))
                default-directory))
         (proj (project-current nil dir)))
    (unless proj
      (error "No project found for %s" (or buffer "current buffer")))
    proj))

(defun gptel-project--get-project-root (&optional buffer)
  "Return the root directory of the current project.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

Projects are typically identified by version control directories (like
.git) or project.el configuration files."
  (let* ((proj (gptel-project--get-project buffer))
         (root (project-root proj)))
    `((content . (((type . "text")
                   (text . ,root))))
      (structuredContent . ((root . ,root))))))

(defun gptel-project--get-project-name (&optional buffer)
  "Return the name of the current project.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

The project name is typically derived from the root directory name."
  (let* ((proj (gptel-project--get-project buffer))
         (name (project-name proj)))
    `((content . (((type . "text")
                   (text . ,name))))
      (structuredContent . ((name . ,name))))))

(defun gptel-project--list-project-buffers (&optional buffer)
  "Return a list of buffer names associated with the current project.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

Returns only buffers that are visiting files within the project
directory structure."
  (let* ((proj (gptel-project--get-project buffer))
         (buffers (project-buffers proj))
         (buffer-names (mapcar #'buffer-name buffers))
         (formatted (string-join buffer-names "\n")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((buffers . ,buffer-names))))))

(defun gptel-project--list-project-files (&optional buffer)
  "Return a list of files associated with the current project.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

Returns all files tracked by the project, respecting ignore patterns
(like .gitignore for Git projects). This may be a large list for big
projects."
  (let* ((proj (gptel-project--get-project buffer))
         (files (project-files proj))
         (formatted (string-join files "\n")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((files . ,files))))))

(defun gptel-project--get-project-info (&optional buffer)
  "Get detailed information about the current project.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

Returns the project name, root directory, number of tracked files, and
number of open buffers associated with the project."
  (let* ((proj (gptel-project--get-project buffer))
         (name (project-name proj))
         (root (project-root proj))
         (files (project-files proj))
         (file-count (length files))
         (buffers (project-buffers proj))
         (buffer-count (length buffers))
         (text (format "Project: %s
Root: %s
Files: %d
Open Buffers: %d"
                       name root file-count buffer-count)))
    `((content . (((type . "text")
                   (text . ,text))))
      (structuredContent . ((name . ,name)
                            (root . ,root)
                            (fileCount . ,file-count)
                            (bufferCount . ,buffer-count))))))

(defun gptel-project--find-file-in-project (pattern &optional buffer)
  "Find files in the current project matching PATTERN.
If BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

PATTERN is matched against file paths relative to the project root.
Returns a list of matching file paths."
  (let* ((proj (gptel-project--get-project buffer))
         (files (project-files proj))
         (matches (seq-filter
                   (lambda (file)
                     (string-match-p pattern file))
                   files))
         (formatted (if matches
                        (string-join matches "\n")
                      "No files found matching pattern")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((matches . ,matches)
                            (count . ,(length matches)))))))

(gptel-make-tool
 :name "get_project_root"
 :function #'gptel-project--get-project-root
 :description
 "Return the root directory of the current project. If BUFFER is
provided, use the buffer identified by BUFFER to determine what the
current project is. Otherwise, use the current buffer.

Projects are typically identified by version control directories (like
.git) or project.el configuration files. Returns an error if no project
is found."
 :category "project"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(gptel-make-tool
 :name "get_project_name"
 :function #'gptel-project--get-project-name
 :description
 "Return the name of the current project. If BUFFER is provided, use the
buffer identified by BUFFER to determine what the current project is.
Otherwise, use the current buffer.

The project name is typically derived from the root directory name.
Returns an error if no project is found."
 :category "project"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(gptel-make-tool
 :name "list_project_buffers"
 :function #'gptel-project--list-project-buffers
 :description
 "Return a list of buffer names associated with the current project. If
BUFFER is provided, use the buffer identified by BUFFER to determine
what the current project is. Otherwise, use the current buffer.

Returns only buffers that are visiting files within the project
directory structure. Returns an error if no project is found."
 :category "project"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(gptel-make-tool
 :name "list_project_files"
 :function #'gptel-project--list-project-files
 :description
 "Return a list of files associated with the current project. If BUFFER
is provided, use the buffer identified by BUFFER to determine what the
current project is. Otherwise, use the current buffer.

Returns all files tracked by the project, respecting ignore patterns
(like .gitignore for Git projects). This may be a large list for big
projects. Returns an error if no project is found."
 :category "project"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(gptel-make-tool
 :name "get_project_info"
 :function #'gptel-project--get-project-info
 :description
 "Get detailed information about the current project. If BUFFER is
provided, use the buffer identified by BUFFER to determine what the
current project is. Otherwise, use the current buffer.

Returns the project name, root directory, number of tracked files, and
number of open buffers associated with the project. This is useful for
getting an overview of the project without retrieving large file lists.
Returns an error if no project is found."
 :category "project"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(gptel-make-tool
 :name "find_file_in_project"
 :function #'gptel-project--find-file-in-project
 :description
 "Find files in the current project matching PATTERN. If BUFFER is
provided, use the buffer identified by BUFFER to determine what the
current project is. Otherwise, use the current buffer.

PATTERN is a regular expression matched against file paths relative to
the project root. Returns a list of matching file paths. Useful for
locating specific files when you don't know their exact path. Returns an
error if no project is found."
 :category "project"
 :args (list (list :name "pattern"
                   :type 'string
                   :description "Regular expression pattern to match against file paths in the project.")
             (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to use for determining the current project. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers."
                   :optional t)))

(provide 'gptel-project)
;;; gptel-project.el ends here
