(require 'org)
(require 'ox-publish)

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-doctype "html5")

(setq org-publish-project-alist
      `(("site-org"
         :base-directory "."
         :base-extension "org"
         :recursive t
         :publishing-function (org-html-publish-to-html)
         :publishing-directory "./public"
         :exclude ,(regexp-opt '("README" "draft"))
         :html-head-extra "<link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\"/>")
        ("site-static"
         :base-directory "."
         :exclude "public/"
         :base-extension ("jpg" "jpeg" "gif" "png" "svg" "ico" "cur" "css" "js"
                           "woff" "html" "pdf")
         :publishing-directory "./public"
         :publishing-function 'org-publish-attachment
         :recursive t)
        ("site"
         :components ("site-org"))))

(org-publish-all)
