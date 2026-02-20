;;; gptel-buffers.el --- GPTel tools for working with buffers -*- lexical-binding: t -*-

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

;; This package provides GPTel tools for working with Emacs buffers,
;; including creating, reading, modifying, and managing buffer state.

;;; Code:

(require 'gptel)

(defun gptel-buffers--create-buffer (name)
  "Create a new buffer using NAME.
If there is already a buffer called NAME, then append a suffix to NAME
so that it becomes unique. Return the actual name of the resulting
buffer."
  (let* ((buffer (generate-new-buffer name))
         (buffer-name (buffer-name buffer))
         (text (format "Successfully created buffer: %s" buffer-name)))
    `((content . (((type . "text")
                   (text . ,text))))
      (structuredContent . ((content . ,text)
                            (bufferName . ,buffer-name))))))

(defun gptel-buffers--list-buffers ()
  "Return a list of currently available buffers."
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (formatted (string-join buffers "\n")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((buffers . ,buffers))))))

(defun gptel-buffers--list-visible-buffers ()
  "Return a list of buffers that are currently displayed to the user."
  (let* ((ws (window-list))
         (bs (mapcar #'window-buffer ws))
         (buffers (mapcar #'buffer-name bs))
         (formatted (string-join buffers "\n")))
    `((content . (((type . "text")
                   (text . ,formatted))))
      (structuredContent . ((buffers . ,buffers))))))

(defun gptel-buffers--read-buffer (buffer &optional start end)
  "Return the text currently stored in BUFFER.
If START is provided, begin reading from that position (1-indexed).
If END is provided, stop reading at that position.
If neither START nor END are provided, return the entire buffer contents."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (let* ((beg (if start (min start (point-max)) (point-min)))
             (end (if end (min end (point-max)) (point-max)))
             (content (buffer-substring-no-properties beg end)))
        `((content . (((type . "text")
                       (text . ,content))))
          (structuredContent . ((content . ,content))))))))

(defun gptel-buffers--insert-into-buffer (buffer text &optional position)
  "Insert TEXT into BUFFER.
If POSITION is specified (1-indexed), insert the text at that location.
Otherwise, insert TEXT at the end of the buffer.
Returns the position where text was inserted."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (save-excursion
        (goto-char (or position (point-max)))
        (insert text))
      (let ((pos (point)))
        (let ((msg (format "Successfully inserted text into %s at position %d" buffer pos)))
          `((content . (((type . "text")
                         (text . ,msg))))
            (structuredContent . ((message . ,msg)
                                  (position . ,pos)))))))))

(defun gptel-buffers--save-buffer (buffer)
  "Save BUFFER to its visited file if modified.
Returns an error if the buffer has no associated file."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (unless (buffer-file-name)
        (error "Buffer %s has no associated file" buffer))
      (save-buffer)
      (let ((msg (format "Successfully saved buffer %s to %s"
                         buffer (buffer-file-name))))
        `((content . (((type . "text")
                       (text . ,msg))))
          (structuredContent . ((message . ,msg)
                                (file . ,(buffer-file-name)))))))))

(defun gptel-buffers--pop-to-buffer (buffer)
  "Display BUFFER to the user.
Switches to the buffer in the current window or another window."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (pop-to-buffer b)
    (let ((msg (format "Successfully displayed buffer: %s" buffer)))
      `((content . (((type . "text")
                     (text . ,msg))))
        (structuredContent . ((message . ,msg)
                              (bufferName . ,buffer)))))))

(defun gptel-buffers--kill-buffer (buffer)
  "Kill (close) BUFFER.
If the buffer is modified and has an associated file, this may prompt
the user to save changes."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (kill-buffer b)
    (let ((msg (format "Successfully killed buffer: %s" buffer)))
      `((content . (((type . "text")
                     (text . ,msg))))
        (structuredContent . ((message . ,msg)
                              (bufferName . ,buffer)))))))

(defun gptel-buffers--get-buffer-info (buffer)
  "Get detailed information about BUFFER.
Returns metadata including file name, modification status, size, major
mode, and whether the buffer is read-only."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (let* ((file-name (buffer-file-name))
             (modified (buffer-modified-p))
             (size (buffer-size))
             (major-mode-name (symbol-name major-mode))
             (read-only buffer-read-only)
             (text (format "Buffer: %s
File: %s
Modified: %s
Size: %d characters
Major Mode: %s
Read-only: %s"
                           buffer
                           (or file-name "none")
                           (if modified "yes" "no")
                           size
                           major-mode-name
                           (if read-only "yes" "no"))))
        `((content . (((type . "text")
                       (text . ,text))))
          (structuredContent . ((bufferName . ,buffer)
                                (fileName . ,file-name)
                                (modified . ,(if modified t :json-false))
                                (size . ,size)
                                (majorMode . ,major-mode-name)
                                (readOnly . ,(if read-only t :json-false)))))))))

(defun gptel-buffers--replace-buffer-content (buffer content)
  "Replace the entire contents of BUFFER with CONTENT.
This erases all existing content in the buffer. Use with caution."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (erase-buffer)
      (insert content))
    (let ((msg (format "Successfully replaced contents of buffer: %s" buffer)))
      `((content . (((type . "text")
                     (text . ,msg))))
        (structuredContent . ((message . ,msg)
                              (bufferName . ,buffer)))))))

(gptel-make-tool
 :name "create_buffer"
 :function #'gptel-buffers--create-buffer
 :description
 "Create a new buffer using NAME. If there is already a buffer called
NAME, then append a suffix to NAME so that it becomes unique. Return the
actual name of the resulting buffer."
 :category "buffers"
 :args (list (list :name "name"
                   :type 'string
                   :description "The name used to create the buffer. If a buffer already exists with the same name, then a suffix will be added to make it unique.")))

(gptel-make-tool
 :name "list_buffers"
 :function #'gptel-buffers--list-buffers
 :description
 "Return a list of currently available buffers. This includes all buffers
in the current Emacs session, whether visible or not."
 :category "buffers"
 :args nil)

(gptel-make-tool
 :name "list_visible_buffers"
 :function #'gptel-buffers--list-visible-buffers
 :description
 "Return a list of buffers that are currently displayed to the user in
windows. This only includes buffers that are actively visible on screen."
 :category "buffers"
 :args nil)

(gptel-make-tool
 :name "read_buffer"
 :function #'gptel-buffers--read-buffer
 :description
 "Return the text currently stored in BUFFER. Optionally specify START and
END positions (1-indexed) to read only a portion of the buffer. If
neither START nor END are provided, returns the entire buffer contents."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to read. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "start"
                   :type 'integer
                   :description "Starting position (1-indexed) to begin reading from. If not provided, starts from the beginning."
                   :optional t)
             (list :name "end"
                   :type 'integer
                   :description "Ending position (1-indexed) to stop reading at. If not provided, reads to the end."
                   :optional t)))

(gptel-make-tool
 :name "insert_into_buffer"
 :function #'gptel-buffers--insert-into-buffer
 :description
 "Insert TEXT into BUFFER. If POSITION is specified (1-indexed), insert
the text at that location. Otherwise, insert TEXT at the end of the
buffer. Returns the position where the text was inserted."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to insert into. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "text"
                   :type 'string
                   :description "The text to be inserted.")
             (list :name "position"
                   :type 'integer
                   :description "Position (1-indexed) where text should be inserted. If not provided, text is inserted at the end of the buffer."
                   :optional t)))

(gptel-make-tool
 :name "save_buffer"
 :function #'gptel-buffers--save-buffer
 :description
 "Save BUFFER to its visited file if modified. Returns an error if the
buffer has no associated file. This is equivalent to the save-file
command in Emacs."
 :category "buffers"
 :confirm t
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to save. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")))

(gptel-make-tool
 :name "pop_to_buffer"
 :function #'gptel-buffers--pop-to-buffer
 :description
 "Display BUFFER to the user. Switches to the buffer in the current window
or displays it in another window, making it visible to the user."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to display. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")))

(gptel-make-tool
 :name "kill_buffer"
 :function #'gptel-buffers--kill-buffer
 :description
 "Kill (close) BUFFER. If the buffer is modified and has an associated
file, this may prompt the user to save changes before closing."
 :category "buffers"
 :confirm t
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to kill. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")))

(gptel-make-tool
 :name "get_buffer_info"
 :function #'gptel-buffers--get-buffer-info
 :description
 "Get detailed information about BUFFER. Returns metadata including file
name (if any), modification status, size in characters, major mode, and
whether the buffer is read-only. Useful for understanding buffer state
without reading its contents."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to get information about. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")))

(gptel-make-tool
 :name "replace_buffer_content"
 :function #'gptel-buffers--replace-buffer-content
 :description
 "Replace the entire contents of BUFFER with CONTENT. This erases all
existing content in the buffer and replaces it with the new content. Use
with caution as this operation cannot be undone programmatically."
 :category "buffers"
 :confirm t
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer whose contents should be replaced. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "content"
                   :type 'string
                   :description "The new content to replace the buffer's current contents with.")))

(provide 'gptel-buffers)
;;; gptel-buffers.el ends here
