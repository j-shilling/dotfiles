;;; gptel-info.el --- GPTel tools for working with Info documentation -*- lexical-binding: t -*-

;; Author: Assistant

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

;; This package provides GPTel tools for looking up contextual information
;; in Info documentation. It enables LLMs to search for topics, read node
;; content, and follow cross-references to gather relevant information.

;;; Code:

(require 'gptel)
(require 'info)

(defun gptel-info--get-info-buffer ()
  "Find and return the first Info buffer, or nil if none exists."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (derived-mode-p 'Info-mode)))
            (buffer-list)))

(defun gptel-info--search-topic (topic &optional manual)
  "Search for TOPIC in Info documentation.
If MANUAL is specified, restrict search to that manual.
Navigate to the first relevant result."
  (let ((Info-history nil)
        (Info-history-list nil)
        (search-manual (or manual "emacs"))
        (case-fold-search t)
        success-msg)
    (condition-case err
        (progn
          ;; Try index lookup first (most reliable)
          (info-initialize)
          (Info-find-node search-manual "Top")
          (Info-index topic)
          (setq success-msg
                (format "Found '%s' in manual '%s', node '%s'"
                        topic search-manual Info-current-node))
          `((content . (((type . "text")
                         (text . ,success-msg))))
            (structuredContent . ((message . ,success-msg)
                                  (file . ,Info-current-file)
                                  (node . ,Info-current-node)
                                  (buffer . ,(buffer-name))))))
      (error
       ;; If index lookup fails, try text search
       (condition-case search-err
           (progn
             (Info-find-node search-manual "Top")
             (Info-search topic)
             (setq success-msg
                   (format "Found '%s' via search in manual '%s', node '%s'"
                           topic search-manual Info-current-node))
             `((content . (((type . "text")
                            (text . ,success-msg))))
               (structuredContent . ((message . ,success-msg)
                                     (file . ,Info-current-file)
                                     (node . ,Info-current-node)
                                     (buffer . ,(buffer-name))))))
         (error
          (let ((error-msg (format "Could not find topic '%s' in manual '%s': %s"
                                   topic search-manual
                                   (error-message-string search-err))))
            `((content . (((type . "text")
                           (text . ,error-msg))))
              (structuredContent . ((error . t)
                                    (message . ,error-msg)))))))))))

(defun gptel-info--read-current-node (&optional buffer-name)
  "Read and return the complete text content of the current Info node.
If BUFFER-NAME is provided, read from that buffer. Otherwise, use the
current buffer or find the first Info buffer."
  (let ((info-buffer (cond
                      (buffer-name (get-buffer buffer-name))
                      ((derived-mode-p 'Info-mode) (current-buffer))
                      (t (gptel-info--get-info-buffer)))))
    (unless info-buffer
      (error "No Info buffer found"))

    (with-current-buffer info-buffer
      (unless (derived-mode-p 'Info-mode)
        (error "Buffer '%s' is not an Info buffer" (buffer-name info-buffer)))

      (let* ((node-name Info-current-node)
             (file-name Info-current-file)
             (content (save-excursion
                        (goto-char (point-min))
                        ;; Skip the header line
                        (forward-line 1)
                        ;; Read from after header to end of buffer
                        (buffer-substring-no-properties (point) (point-max))))
             (summary (format "Content of node '%s' in '%s' (%d characters)"
                              node-name
                              (if (stringp file-name)
                                  (file-name-nondirectory file-name)
                                file-name)
                              (length content))))
        `((content . (((type . "text")
                       (text . ,(concat summary "\n\n" content)))))
          (structuredContent . ((node . ,node-name)
                                (file . ,file-name)
                                (buffer . ,(buffer-name info-buffer))
                                (content . ,content)
                                (length . ,(length content)))))))))

(defun gptel-info--index-lookup (topic &optional manual)
  "Look up TOPIC in the index of MANUAL (defaults to 'emacs').
Navigate to the first matching index entry."
  (let ((Info-history nil)
        (Info-history-list nil)
        (search-manual (or manual "emacs"))
        (case-fold-search t))
    (condition-case err
        (progn
          (info-initialize)
          (Info-find-node search-manual "Top")
          (Info-index topic)
          (let ((msg (format "Found index entry for '%s' in manual '%s', node '%s'"
                             topic search-manual Info-current-node)))
            `((content . (((type . "text")
                           (text . ,msg))))
              (structuredContent . ((message . ,msg)
                                    (file . ,Info-current-file)
                                    (node . ,Info-current-node)
                                    (buffer . ,(buffer-name))
                                    (topic . ,topic))))))
      (error
       (let ((error-msg (format "No index entry for '%s' in manual '%s': %s"
                                topic search-manual
                                (error-message-string err))))
         `((content . (((type . "text")
                        (text . ,error-msg))))
           (structuredContent . ((error . t)
                                 (message . ,error-msg)
                                 (topic . ,topic)))))))))

(defun gptel-info--follow-reference (reference-name &optional buffer-name)
  "Follow a cross-reference named REFERENCE-NAME from the current node.
If BUFFER-NAME is provided, use that buffer. Otherwise, use the current
buffer or find the first Info buffer."
  (let ((info-buffer (cond
                      (buffer-name (get-buffer buffer-name))
                      ((derived-mode-p 'Info-mode) (current-buffer))
                      (t (gptel-info--get-info-buffer)))))
    (unless info-buffer
      (error "No Info buffer found"))

    (with-current-buffer info-buffer
      (unless (derived-mode-p 'Info-mode)
        (error "Buffer '%s' is not an Info buffer" (buffer-name info-buffer)))

      (let ((old-node Info-current-node)
            (old-file Info-current-file))
        (condition-case err
            (progn
              (Info-follow-reference reference-name)
              (let ((msg (format "Followed reference '%s' from node '%s' to node '%s'"
                                 reference-name old-node Info-current-node)))
                `((content . (((type . "text")
                               (text . ,msg))))
                  (structuredContent . ((message . ,msg)
                                        (from-node . ,old-node)
                                        (to-node . ,Info-current-node)
                                        (file . ,Info-current-file)
                                        (buffer . ,(buffer-name info-buffer))
                                        (reference . ,reference-name))))))
          (error
           (let ((error-msg (format "Could not follow reference '%s': %s"
                                    reference-name
                                    (error-message-string err))))
             `((content . (((type . "text")
                            (text . ,error-msg))))
               (structuredContent . ((error . t)
                                     (message . ,error-msg)
                                     (reference . ,reference-name)))))))))))

(defun gptel-info--list-references (&optional buffer-name)
  "List all cross-references available in the current Info node.
If BUFFER-NAME is provided, list references from that buffer. Otherwise,
use the current buffer or find the first Info buffer."
  (let ((info-buffer (cond
                      (buffer-name (get-buffer buffer-name))
                      ((derived-mode-p 'Info-mode) (current-buffer))
                      (t (gptel-info--get-info-buffer)))))
    (unless info-buffer
      (error "No Info buffer found"))

    (with-current-buffer info-buffer
      (unless (derived-mode-p 'Info-mode)
        (error "Buffer '%s' is not an Info buffer" (buffer-name info-buffer)))

      (let ((references '())
            (case-fold-search t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\*note[ \n\t]+\\([^:]+\\):" nil t)
            (let ((ref-name (match-string-no-properties 1)))
              ;; Clean up whitespace
              (setq ref-name (replace-regexp-in-string "[ \n\t]+" " " ref-name))
              (push ref-name references))))

        (setq references (nreverse references))

        (let ((msg (if references
                       (format "Found %d reference(s) in node '%s':\n%s"
                               (length references)
                               Info-current-node
                               (mapconcat (lambda (ref) (concat "  - " ref))
                                          references "\n"))
                     (format "No references found in node '%s'" Info-current-node))))
          `((content . (((type . "text")
                         (text . ,msg))))
            (structuredContent . ((node . ,Info-current-node)
                                  (file . ,Info-current-file)
                                  (buffer . ,(buffer-name info-buffer))
                                  (references . ,(vconcat references))
                                  (count . ,(length references))))))))))

;; Register tools with gptel

(gptel-make-tool
 :name "info_search_topic"
 :function #'gptel-info--search-topic
 :description
 "Search for a topic across Info documentation and navigate to the most relevant result.
This is the primary entry point for looking up information. It first tries to find
the topic in the manual's index (most reliable), and falls back to text search if needed."
 :category "info"
 :args (list (list :name "topic"
                   :type 'string
                   :description "The topic or term to search for in the documentation.")
             (list :name "manual"
                   :type 'string
                   :optional t
                   :description "Optional manual name to restrict search to (e.g., 'emacs', 'elisp', 'org'). Defaults to 'emacs'.")))

(gptel-make-tool
 :name "info_read_current_node"
 :function #'gptel-info--read-current-node
 :description
 "Read and return the complete text content of the current Info node.
Use this after navigating to a node to extract the actual documentation content.
The content includes all text from the node but excludes the header line."
 :category "info"
 :args (list (list :name "buffer_name"
                   :type 'string
                   :optional t
                   :description "Name of the Info buffer to read from. If not provided, uses the current buffer or finds the first Info buffer.")))

(gptel-make-tool
 :name "info_index_lookup"
 :function #'gptel-info--index-lookup
 :description
 "Look up a topic in a manual's index and navigate to the corresponding entry.
This is the most reliable way to find documentation for a specific topic.
The index is curated by the manual authors to point to the primary documentation
for each topic."
 :category "info"
 :args (list (list :name "topic"
                   :type 'string
                   :description "The topic to look up in the index.")
             (list :name "manual"
                   :type 'string
                   :optional t
                   :description "Manual to search in (e.g., 'emacs', 'elisp', 'org'). Defaults to 'emacs'.")))

(gptel-make-tool
 :name "info_follow_reference"
 :function #'gptel-info--follow-reference
 :description
 "Follow a cross-reference from the current node to get related information.
Cross-references link to related topics and provide additional context.
Use info_list_references first to see what references are available."
 :category "info"
 :args (list (list :name "reference_name"
                   :type 'string
                   :description "The name of the cross-reference to follow. Must match a reference from the current node.")
             (list :name "buffer_name"
                   :type 'string
                   :optional t
                   :description "Name of the Info buffer to use. If not provided, uses the current buffer or finds the first Info buffer.")))

(gptel-make-tool
 :name "info_list_references"
 :function #'gptel-info--list-references
 :description
 "List all cross-references available in the current Info node.
Use this to discover what related topics you can explore from the current location.
Returns a list of reference names that can be used with info_follow_reference."
 :category "info"
 :args (list (list :name "buffer_name"
                   :type 'string
                   :optional t
                   :description "Name of the Info buffer to list references from. If not provided, uses the current buffer or finds the first Info buffer.")))

(provide 'gptel-info)
;;; gptel-info.el ends here
