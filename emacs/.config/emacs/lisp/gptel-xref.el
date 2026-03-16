;;; gptel-xref.el --- GPTel tools for working with Xref -*- lexical-binding: t -*-

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

;; This package provides GPTel tools for integrating with Emacs's xref
;; (cross-reference) package.

;;; Code:

(require 'gptel)
(require 'xref)

(defun gptel-xref--buffer (&optional buffer)
  "Return BUFFER (a buffer name) as a live buffer, or current buffer."
  (let ((buf (if buffer (get-buffer buffer) (current-buffer))))
    (unless (buffer-live-p buf)
      (error "No live buffer: %s" buffer))
    buf))

(defun gptel-xref--location->alist (loc)
  "Convert xref-item/location LOC to an alist.
LOC can be an `xref-item' or an `xref-location'."
  (let* ((location (if (xref-item-p loc) (xref-item-location loc) loc))
         (summary (and (xref-item-p loc) (xref-item-summary loc)))
         (group (ignore-errors (xref-location-group location)))
         (marker (xref-location-marker location)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((file (or (buffer-file-name) group (buffer-name)))
               (line (line-number-at-pos))
               (col (1+ (current-column)))
               (line-text (string-trim-right
                           (or (thing-at-point 'line t) "")))
               (sum (or summary line-text)))
          `((file . ,file)
            (buffer . ,(buffer-name (current-buffer)))
            (position . ,(marker-position marker))
            (line . ,line)
            (column . ,col)
            (summary . ,sum)))))))

(defun gptel-xref--read-results (fetcher)
  "Call FETCHER to obtain xref items and return (ITEMS . BACKEND)."
  (let ((backend (xref-find-backend)))
    (unless backend
      (error "No xref backend available"))
    (cons (funcall fetcher backend) backend)))

(defun gptel-xref--goto-location (loc display-mode)
  "Jump to LOC (xref-item or xref-location) using DISPLAY-MODE.
DISPLAY-MODE is one of nil/"current", "other-window", "other-frame".

This intentionally avoids internal xref functions that expect interactive
calling context."
  (let* ((location (if (xref-item-p loc) (xref-item-location loc) loc))
         (marker (xref-location-marker location))
         (target-buffer (marker-buffer marker)))
    (unless (buffer-live-p target-buffer)
      (error "Target buffer is not live"))
    (pcase display-mode
      ((or 'nil "current") (switch-to-buffer target-buffer))
      ("other-window" (switch-to-buffer-other-window target-buffer))
      ("other-frame" (switch-to-buffer-other-frame target-buffer))
      (_ (error "Invalid display_mode: %S" display-mode)))
    (goto-char marker)
    (recenter)
    t))

(defun gptel-xref--goto-definition (identifier &optional buffer display-mode)
  "Navigate to definition of IDENTIFIER.
BUFFER optionally specifies the source buffer name.
DISPLAY-MODE is one of "current", "other-window", "other-frame"."
  (with-current-buffer (gptel-xref--buffer buffer)
    (condition-case err
        (pcase-let* ((`(,items . ,_backend)
                      (gptel-xref--read-results
                       (lambda (backend)
                         (xref-backend-definitions backend identifier))))
                     (item (car items)))
          (unless item
            (error "No definitions found for %S" identifier))
          ;; Jump to first definition.
          (gptel-xref--goto-location item display-mode)
          (let ((loc (gptel-xref--location->alist item))
                (msg (format "Navigated to definition of %s" identifier)))
            `((content . (((type . "text") (text . ,msg))))
              (structuredContent . ((success . t)
                                    (message . ,msg)
                                    (location . ,loc))))))
      (error
       (let ((msg (format "xref_goto_definition failed: %s" (error-message-string err))))
         `((content . (((type . "text") (text . ,msg))))
           (structuredContent . ((success . :json-false)
                                 (error . t)
                                 (message . ,msg)))))))))

(defun gptel-xref--goto-reference (identifier index &optional buffer)
  "Navigate to a specific reference of IDENTIFIER by INDEX (0-based)."
  (with-current-buffer (gptel-xref--buffer buffer)
    (condition-case err
        (pcase-let* ((`(,items . ,_backend)
                      (gptel-xref--read-results
                       (lambda (backend)
                         (xref-backend-references backend identifier))))
                     (n (length items)))
          (unless (> n 0)
            (error "No references found for %S" identifier))
          (unless (and (integerp index) (<= 0 index) (< index n))
            (error "Index out of range: %s (have %d references)" index n))
          (let ((item (nth index items)))
            (gptel-xref--goto-location item nil)
            (let ((loc (gptel-xref--location->alist item))
                  (msg (format "Navigated to reference %d of %s" index identifier)))
              `((content . (((type . "text") (text . ,msg))))
                (structuredContent . ((success . t)
                                      (message . ,msg)
                                      (location . ,loc)
                                      (index . ,index)
                                      (count . ,n)))))))
      (error
       (let ((msg (format "xref_goto_reference failed: %s" (error-message-string err))))
         `((content . (((type . "text") (text . ,msg))))
           (structuredContent . ((success . :json-false)
                                 (error . t)
                                 (message . ,msg)))))))))

(defun gptel-xref--go-back ()
  "Go back in xref navigation history."
  (condition-case err
      (progn
        (xref-go-back)
        (let ((loc `((buffer . ,(buffer-name))
                     (file . ,(buffer-file-name))
                     (position . ,(point))
                     (line . ,(line-number-at-pos))
                     (column . ,(1+ (current-column)))
                     (summary . ,(string-trim-right (or (thing-at-point 'line t) "")))))
              (msg "Went back in xref history"))
          `((content . (((type . "text") (text . ,msg))))
            (structuredContent . ((success . t)
                                  (message . ,msg)
                                  (location . ,loc))))))
    (error
     (let ((msg (format "xref_go_back failed: %s" (error-message-string err))))
       `((content . (((type . "text") (text . ,msg))))
         (structuredContent . ((success . :json-false)
                               (error . t)
                               (message . ,msg))))))))

(defun gptel-xref--go-forward ()
  "Go forward in xref navigation history."
  (condition-case err
      (progn
        (xref-go-forward)
        (let ((loc `((buffer . ,(buffer-name))
                     (file . ,(buffer-file-name))
                     (position . ,(point))
                     (line . ,(line-number-at-pos))
                     (column . ,(1+ (current-column)))
                     (summary . ,(string-trim-right (or (thing-at-point 'line t) "")))))
              (msg "Went forward in xref history"))
          `((content . (((type . "text") (text . ,msg))))
            (structuredContent . ((success . t)
                                  (message . ,msg)
                                  (location . ,loc))))))
    (error
     (let ((msg (format "xref_go_forward failed: %s" (error-message-string err))))
       `((content . (((type . "text") (text . ,msg))))
         (structuredContent . ((success . :json-false)
                               (error . t)
                               (message . ,msg))))))))

;; Register tools

(gptel-make-tool
 :name "xref_goto_definition"
 :function #'gptel-xref--goto-definition
 :description
 "Navigate to definition of identifier."
 :category "xref"
 :args (list (list :name "identifier"
                   :type 'string
                   :description "Identifier to find definitions for.")
             (list :name "buffer"
                   :type 'string
                   :optional t
                   :description "Source buffer name (defaults to current).")
             (list :name "display_mode"
                   :type 'string
                   :optional t
                   :description "Where to display target: \"current\", \"other-window\", or \"other-frame\".")))

(gptel-make-tool
 :name "xref_goto_reference"
 :function #'gptel-xref--goto-reference
 :description
 "Navigate to a specific reference (by index) of an identifier."
 :category "xref"
 :args (list (list :name "identifier"
                   :type 'string
                   :description "Identifier to find references for.")
             (list :name "index"
                   :type 'integer
                   :description "0-based index of the reference to jump to.")
             (list :name "buffer"
                   :type 'string
                   :optional t
                   :description "Source buffer name (defaults to current).")))

(gptel-make-tool
 :name "xref_go_back"
 :function #'gptel-xref--go-back
 :description
 "Go back in xref navigation history."
 :category "xref"
 :args nil)

(gptel-make-tool
 :name "xref_go_forward"
 :function #'gptel-xref--go-forward
 :description
 "Go forward in xref navigation history."
 :category "xref"
 :args nil)

(provide 'gptel-xref)
;;; gptel-xref.el ends here
