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

;; TODO: Future Enhancements
;;
;; The following features are planned for future implementation:
;;
;; 11. Add Buffer List Filtering
;;     Enhance `list_buffers' with filtering options:
;;     - Filter by major mode
;;     - Filter by modified status
;;     - Filter by file-visiting status
;;     - Exclude internal buffers (those starting with space or *)
;;
;; 12. Add Undo/Redo Support
;;     - `undo_buffer' - Undo last change
;;     - `redo_buffer' - Redo undone change
;;     - `buffer_undo_list_length' - Get number of undo steps available
;;
;; 3. Add Point/Cursor Position Tools
;;    The manual emphasizes point as fundamental to buffer operations:
;;    - `get_point' - Get current cursor position in buffer
;;    - `goto_position' - Move point to specific position
;;    - `get_point_min' / `get_point_max' - Get accessible region bounds
;;
;; 4. Add Narrowing Support
;;    Narrowing is a powerful Emacs feature for restricting buffer operations:
;;    - `narrow_to_region' - Limit accessible portion of buffer
;;    - `widen' - Remove narrowing restrictions
;;    - `buffer_narrowed_p' - Check if buffer is narrowed
;;
;; 5. Add Indirect Buffer Support
;;    Indirect buffers share text but have independent point/narrowing:
;;    - `make_indirect_buffer' - Create indirect buffer sharing text with base buffer
;;    - `clone_indirect_buffer' - Clone current buffer as indirect
;;    - `get_base_buffer' - Get base buffer of indirect buffer

;;; Code:

(require 'gptel)

(defun gptel-buffers--create-buffer (name &optional mode)
  "Create a new buffer using NAME and initialize it with the appropriate mode.
If there is already a buffer called NAME, then append a suffix to NAME
so that it becomes unique.

The major mode is determined in the following order:
1. If MODE is provided and is a valid mode function, use it
2. If MODE is provided as a string (file extension), find the mode for that extension
3. Try to auto-detect mode from buffer NAME using `auto-mode-alist'
4. Fall back to `text-mode'

MODE can be:
  - A major mode function symbol (e.g., `python-mode', `markdown-mode')
  - A string representing a file extension (e.g., \"py\", \"md\", \"js\")
  - nil (auto-detect from name, or default to `text-mode')

Return the actual name of the resulting buffer."
  (let* ((buffer (generate-new-buffer name))
         (buffer-name (buffer-name buffer))
         (resolved-mode nil))

    ;; Initialize the buffer with the appropriate mode
    (with-current-buffer buffer
      (cond
       ;; Case 1: MODE is a valid mode function symbol
       ((and mode (symbolp mode) (fboundp mode))
        (setq resolved-mode mode)
        (funcall mode))

       ;; Case 2: MODE is a string (file extension)
       ((stringp mode)
        (let* ((ext-with-dot (if (string-prefix-p "." mode)
                                 mode
                               (concat "." mode)))
               (fake-filename (concat "file" ext-with-dot))
               (mode-entry (assoc-default fake-filename auto-mode-alist #'string-match)))
          (if (and mode-entry (symbolp mode-entry) (fboundp mode-entry))
              (progn
                (setq resolved-mode mode-entry)
                (funcall mode-entry))
            ;; Extension didn't match, fall back to text-mode
            (text-mode)
            (setq resolved-mode 'text-mode))))

       ;; Case 3: No MODE provided, try auto-detection from buffer name
       (t
        (let ((mode-entry (assoc-default buffer-name auto-mode-alist #'string-match)))
          (if (and mode-entry (symbolp mode-entry) (fboundp mode-entry))
              (progn
                (setq resolved-mode mode-entry)
                (funcall mode-entry))
            ;; No match found, use text-mode
            (text-mode)
            (setq resolved-mode 'text-mode))))))

    (let ((text (format "Successfully created buffer: %s (mode: %s)"
                       buffer-name resolved-mode)))
      `((content . (((type . "text")
                     (text . ,text))))
        (structuredContent . ((content . ,text)
                              (bufferName . ,buffer-name)
                              (mode . ,(symbol-name resolved-mode))))))))

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
If END is provided, stop reading at that position (not included in result).
If neither START nor END are provided, return the entire buffer contents.

IMPORTANT: Positions are 1-indexed and represent locations BETWEEN characters:
  - Position 1 is BEFORE the first character
  - Position N is BETWEEN character N-1 and character N
  - For a buffer with N characters, position N+1 is AFTER the last character
  - Reading from position A to position B returns characters at positions A through B-1

START and END must be within the accessible portion of the buffer (which may
be restricted by narrowing). Use get_buffer_info to check buffer bounds."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (let* ((buf-min (point-min))
             (buf-max (point-max))
             (beg (if start
                      (progn
                        (unless (and (integerp start) (>= start 1))
                          (error "START position must be a positive integer, got %s" start))
                        (when (< start buf-min)
                          (error "START position %d is before buffer start %d" start buf-min))
                        (when (> start buf-max)
                          (error "START position %d is after buffer end %d" start buf-max))
                        start)
                    buf-min))
             (end (if end
                      (progn
                        (unless (and (integerp end) (>= end 1))
                          (error "END position must be a positive integer, got %s" end))
                        (when (< end buf-min)
                          (error "END position %d is before buffer start %d" end buf-min))
                        (when (> end buf-max)
                          (error "END position %d is after buffer end %d" end buf-max))
                        (when (< end beg)
                          (error "END position %d is before START position %d" end beg))
                        end)
                    buf-max))
             (content (buffer-substring-no-properties beg end)))
        `((content . (((type . "text")
                       (text . ,content))))
          (structuredContent . ((content . ,content)
                                (start . ,beg)
                                (end . ,end)
                                (bufferMin . ,buf-min)
                                (bufferMax . ,buf-max)))))))

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

(defun gptel-buffers--rename-buffer (buffer new-name &optional unique)
  "Rename BUFFER to NEW-NAME.
If UNIQUE is non-nil, automatically make the name unique if it already
exists by appending a suffix. If UNIQUE is nil and the name already
exists, an error is raised.
Returns the actual new name of the buffer."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      (let ((actual-name (if unique
                             (generate-new-buffer-name new-name)
                           (if (get-buffer new-name)
                               (error "Buffer named %s already exists" new-name)
                             new-name))))
        (rename-buffer actual-name)
        (let ((msg (format "Successfully renamed buffer %s to %s"
                           buffer actual-name)))
          `((content . (((type . "text")
                         (text . ,msg))))
            (structuredContent . ((message . ,msg)
                                  (oldName . ,buffer)
                                  (newName . ,actual-name)))))))))

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

(defun gptel-buffers--set-visited-file-name (buffer filename &optional no-query)
  "Associate BUFFER with FILENAME.
The buffer will be saved to this file when saved. If the file already
exists and NO-QUERY is nil, an error is raised. If NO-QUERY is non-nil,
proceed even if the file exists.
Returns the filename that was set."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (with-current-buffer b
      ;; Expand the filename to get the full path
      (let ((expanded-filename (expand-file-name filename)))
        ;; Check if file exists and we should error
        (when (and (file-exists-p expanded-filename)
                   (not no-query))
          (error "File %s already exists. Set no_query to true to proceed anyway"
                 expanded-filename))
        ;; Set the visited file name
        ;; Pass t as second argument to avoid renaming the buffer
        (set-visited-file-name expanded-filename t)
        (let ((msg (format "Successfully associated buffer %s with file %s"
                           buffer expanded-filename)))
          =((content . (((type . "text")
                         (text . ,msg))))
            (structuredContent . ((message . ,msg)
                                  (bufferName . ,buffer)
                                  (fileName . ,expanded-filename)))))))))

(defun gptel-buffers--read-buffer-lines (buffer start-line end-line)
  "Read specific lines from BUFFER.
START-LINE and END-LINE are 1-indexed line numbers. Both the start and end
lines are included in the result. If END-LINE is greater than the number of
lines in the buffer, reads to the end of the buffer.

Returns the text of the specified lines including their newline characters
(except possibly the last line if the buffer doesn't end with a newline)."
  (let ((b (get-buffer buffer)))
    (unless (buffer-live-p b)
      (error "Buffer %s is not live" buffer))
    (unless (and (integerp start-line) (>= start-line 1))
      (error "START-LINE must be a positive integer, got %s" start-line))
    (unless (and (integerp end-line) (>= end-line 1))
      (error "END-LINE must be a positive integer, got %s" end-line))
    (when (> start-line end-line)
      (error "START-LINE %d is greater than END-LINE %d" start-line end-line))
    (with-current-buffer b
      (save-excursion
        (save-restriction
          (widen)  ; Temporarily remove narrowing to count lines accurately
          (let* ((total-lines (count-lines (point-min) (point-max)))
                 ;; Add 1 if buffer doesn't end with newline
                 (total-lines (if (and (> (point-max) (point-min))
                                      (not (eq (char-before (point-max)) ?\n)))
                                  (1+ total-lines)
                                total-lines)))
            ;; Validate start-line
            (when (> start-line total-lines)
              (error "START-LINE %d exceeds buffer line count %d"
                     start-line total-lines))
            ;; Clamp end-line to total-lines (allow reading to end)
            (let ((actual-end-line (min end-line total-lines)))
              ;; Go to start line
              (goto-char (point-min))
              (forward-line (1- start-line))
              (let ((start-pos (point)))
                ;; Go to end line
                (forward-line (- actual-end-line start-line))
                ;; Move to end of line to include the entire line
                (end-of-line)
                ;; If not at end of buffer and there's a newline, include it
                (when (and (not (eobp)) (eq (char-after) ?\n))
                  (forward-char))
                (let ((end-pos (point))
                      (content (buffer-substring-no-properties start-pos (point))))
                  (let ((msg (format "Read lines %d-%d from buffer %s (%d lines total)"
                                   start-line actual-end-line buffer total-lines)))
                    `((content . (((type . "text")
                                   (text . ,content))))
                      (structuredContent . ((content . ,content)
                                           (message . ,msg)
                                           (startLine . ,start-line)
                                           (endLine . ,actual-end-line)
                                           (totalLines . ,total-lines)
                                           (startPos . ,start-pos)
                                           (endPos . ,end-pos))))))))))))))

(gptel-make-tool
 :name "create_buffer"
 :function #'gptel-buffers--create-buffer
 :description
 "Create a new buffer using NAME with an optional MODE. If there is already
a buffer called NAME, then append a suffix to NAME so that it becomes unique.
Return the actual name of the resulting buffer. The mode is determined by:
1) Using MODE if it's a valid mode function symbol
2) Finding the mode for MODE if it's a file extension string
3) Auto-detecting from the buffer NAME using set-auto-mode
4) Falling back to text-mode if auto-detection fails"
 :category "buffers"
 :args (list (list :name "name"
                   :type 'string
                   :description "The name used to create the buffer. If a buffer already exists with the same name, then a suffix will be added to make it unique. Can include file extension for auto-detection (e.g., 'script.py').")
             (list :name "mode"
                   :type 'string
                   :optional t
                   :description "Optional major mode hint. Can be a mode name (e.g., 'python-mode', 'org-mode') or a file extension (e.g., 'py', 'md', 'js'). If not specified, mode will be auto-detected from the buffer name.")))

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
END positions to read only a portion of the buffer. If neither START nor END
are provided, returns the entire buffer contents.

IMPORTANT: Positions in Emacs are 1-indexed and represent locations BETWEEN
characters, not the characters themselves:
  - Position 1 is BEFORE the first character
  - Position 2 is BETWEEN the first and second characters
  - If a buffer contains 'hello' (5 characters), valid positions are 1-6
  - Position 6 is AFTER the last character
  - To read the entire buffer, use START=1 and END=(buffer size + 1)
  - To read the first character, use START=1 and END=2
  - To read from position N to the end, omit END or set it to (buffer size + 1)

The accessible region of a buffer may be restricted by narrowing. Use
get_buffer_info to see the current buffer size and bounds."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to read. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "start"
                   :type 'integer
                   :description "Starting position (1-indexed) to begin reading from. Position 1 is BEFORE the first character. If not provided, starts from the beginning of the accessible region."
                   :optional t)
             (list :name "end"
                   :type 'integer
                   :description "Ending position (1-indexed) to stop reading at. This position is NOT included in the result (reads up to but not including this position). If not provided, reads to the end of the accessible region."
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

(gptel-make-tool
 :name "rename_buffer"
 :function #'gptel-buffers--rename-buffer
 :description
 "Rename BUFFER to NEW-NAME. If UNIQUE is true, automatically make the name
unique if it already exists by appending a suffix (e.g., 'buffer<2>'). If
UNIQUE is false and the name already exists, an error is raised. Returns
the actual new name of the buffer."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to rename. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "new_name"
                   :type 'string
                   :description "The new name for the buffer.")
             (list :name "unique"
                   :type 'boolean
                   :optional t
                   :description "If true, automatically make the name unique if it already exists. If false (default), raise an error if the name is already in use.")))

(gptel-make-tool
 :name "set_visited_file_name"
 :function #'gptel-buffers--set-visited-file-name
 :description
 "Associate BUFFER with FILENAME. The buffer will be saved to this file when
saved. This is useful for giving a file name to a buffer that was created
without one, or for changing which file a buffer is associated with. If the
file already exists, the user may be prompted for confirmation unless
NO_QUERY is true."
 :category "buffers"
 :confirm t
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to associate with a file. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "filename"
                   :type 'string
                   :description "Path to the file to associate with the buffer. Can be relative or absolute. The buffer will be saved to this location when save_buffer is called.")
             (list :name "no_query"
                   :type 'boolean
                   :optional t
                   :description "If true, proceed without asking for confirmation even if the file already exists. If false (default), may prompt the user if the file exists.")))

(gptel-make-tool
 :name "read_buffer_lines"
 :function #'gptel-buffers--read-buffer-lines
 :description
 "Read specific lines from BUFFER by line number. This is more convenient than
read_buffer when you want to extract lines by their line numbers rather than
character positions.

Lines are 1-indexed (first line is line 1). Both START_LINE and END_LINE are
inclusive - they are both included in the result. For example, to read just
line 5, use START_LINE=5 and END_LINE=5. To read lines 10-20, use START_LINE=10
and END_LINE=20.

If END_LINE exceeds the number of lines in the buffer, reads to the end of the
buffer without error. This makes it easy to read 'from line N to the end' by
using a large END_LINE value.

The returned text includes newline characters (except possibly the last line if
the buffer doesn't end with a newline). Use get_buffer_info to see the total
buffer size, though this tool will tell you the total line count in its response."
 :category "buffers"
 :args (list (list :name "buffer"
                   :type 'string
                   :description "Name of the buffer to read lines from. This can be the return value of create_buffer or one of the names returned by list_buffers or list_visible_buffers.")
             (list :name "start_line"
                   :type 'integer
                   :description "Starting line number (1-indexed, first line is 1). This line will be included in the result.")
             (list :name "end_line"
                   :type 'integer
                   :description "Ending line number (1-indexed, inclusive). This line will be included in the result. If this exceeds the buffer's line count, reads to the end of the buffer.")))

(provide 'gptel-buffers)
;;; gptel-buffers.el ends here
