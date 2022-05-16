;;; publish.el --- Build dotfiles website

;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;; Copyright (C) 2021 David Wilson <david@daviwil.com>
;; Copyright (C) 2022 Jake Shilling <shilling.jake@gmail.com

;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: hypermedia, blog, feed, rss

;; This file is not part of GNU Emacs.

;; This file is loosely based on David Wilson's publish.el, here's his
;; authorship details:

;; Author: David Wilson <david@daviwil.com>
;; Maintainer: David Wilson <david@daviwil.com>
;; URL: https://sr.ht/~daviwil/dotfiles

;; This file is loosely based on Pierre Neidhardt's publish.el, here's his
;; authorship details:

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/ambrevar.gitlab.io

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Docs License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Docs License for more details.
;;
;; You should have received a copy of the GNU General Docs License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage:
;; emacs -Q --batch -l ./publish.el --funcall dw/publish
;;
;;; Code:

;; Initialize package sources
(require 'package)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package esxml
  :ensure t)

(use-package htmlize
  :ensure t)

(require 'esxml)
(require 'ox-publish)

(defun publish-header (info)
  `((header nil
            (div ((class . "container"))
                 (div ((class . "row align-items-center justify-content-between"))
                      (div ((class . "col-sm-12 col-md-8"))
                           "Jake's Dotfiles")
                      (div ((class . "col-sm col-md text-sm-left text-md-right text-lg-right text-xl-right"))
                           "Some doffiles")))
            (div ((class . "container"))
                 (div ((class .  "row align-items-center justify-content-between"))
                      (div ((class .  "col-sm-12 col-md-12"))
                           (nav ((class . "nav"))
                                (a ((class . "nav-link")
                                    (href . "/desktop"))
                                   "Desktop"))))))))

(defun publish-footer (info)
  `((footer ((class . "blog-footer"))
            (div ((class . "container"))
                 (div ((class . "row"))
                      (div ((class . "col-sm col-md text-sm-left text-md-right text-lg-right text-xl-right"))
                           (raw-string ,(concat "Made with " (plist-get info :creator)))))))
    (script ((src . "/js/bootstrap.bundle.min.js")))))

(defun publish-html-template (contents info)
  (concat
   "<!DOCTYPE html>"
   (esxml-to-xml
    `(html nil
           (head nil
                 (title nil
                        ,(dw/strip-html (org-export-data (plist-get info :title) info)))
                 (meta ((charset . "utf-8")
                        (author . "Jake Shilling")
                        (viewport .  "width=device-width, initial-scale=1, shrink-to-fit=no")))
                 (link ((rel . "stylesheet")
                        (href . "/css/bootstrap.min.css"))))
           (body nil
                 ,@(publish-header info)
                 (div ((class . "container"))
                      (div ((class . "row"))
                           (div ((class . "col-sm-12"))
                                (h1 nil
                                    ,(org-export-data (plist-get info :title) info))
                                (p nil
                                   ,(org-export-data (org-export-get-date info "%B %e, %Y") info))
                                (raw-string ,contents))))
                 ,@(publish-footer info))))))

(org-export-define-derived-backend 'publish 'html
  :translate-alist
  '((template . publish-html-template))
  :options-alist
  '((:page-type "PAGE-TYPE" nil nil t)
    (:html-use-infojs nil nil nil)))

(defun dw/strip-html (s)
  "Remove all HTML tags from S."
  (let ((result s))
    (while (string-match "<[^<]*>" result)
      (setq result (replace-match "" nil nil result)))
    result))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/README.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))
          article-dir))))

(defun publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 (concat article-path "index" extension))))
      (org-publish-org-to 'publish
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

;; We're using Git, we don't need no steenking backups
(setq make-backup-files nil)

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-export-with-toc t)

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-preamble nil
      org-html-postamble nil
      org-html-checkbox-type 'html
      org-html-htmlize-output-type 'css
      org-html-self-link-headlines t
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-doctype "html5"
      org-html-indent t)

(setq org-publish-project-alist
      `(("site-org"
         :base-directory "."
         :base-extension "org"
         :recursive t
         :publishing-function (publish-to-html)
         :publishing-directory "./public"
         :with-title nil)
        ("site-static"
         :base-directory "."
         :exclude "public/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "./public"
         :publishing-function (org-publish-attachment)
         :recursive t)
        ("site"
         :components ("site-org" "site-static"))))

(defun dw/publish ()
  (interactive)
  (org-publish-all t))

(provide 'publish)
;;; publish.el ends here
