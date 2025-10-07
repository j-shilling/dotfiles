;;; init-ai.el --- Configuration for gptel           -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

(use-package gptel
  :if (package-installed-p 'gptel)
  :preface

  (defun init-get-anthropic-key ()
    (password-store-get "anthropic-api-key"))

  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :models '(claude-sonnet-4-5-20250929)
                        :key #'init-get-anthropic-key))

  (gptel-make-tool
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args '((:name "buffer"
                  :type string
                  :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (patch)
               (let ((b (generate-new-buffer "*gptel patch*")))
                 (with-current-buffer b
                   (insert patch)
                   (diff-mode))
                 (ediff-patch-buffer 2 b)))
   :name "create_patch_buffer"
   :description "Send a patch to the user so that they can apply or reject changes"
   :args '((:name "patch"
                  :type string
                  :description "A string containing the patch in standard diff format"))
   :category "emacs")

  (gptel-make-tool
   :function (lambda ()
               (project-buffers (project-current)))
   :name "get_project_buffers"
   :description "Return a list of buffers associated with the current project"
   :args '()
   :category "project")

  (gptel-make-tool
   :function (lambda ()
               (project-buffers (project-current)))
   :name "get_project_files"
   :description "Return a list of files associated with the current project"
   :args '()
   :category "project")

  (gptel-make-preset "effect-backend"
    :system 'effect-backend
    :tools '("read_buffer"
             "create_patch_buffer"
             "get_effect_doc"
             "effect_docs_search"
             "get_project_buffers"
             "get_project_files")))

(use-package gptel-prompts
  :if (package-installed-p 'gptel-prompts)
  :after gptel
  :custom
  (gptel-prompts-directory (expand-file-name "prompts" user-emacs-directory))
  :config
  (when (file-exists-p gptel-prompts-directory)
    (gptel-prompts-update)))

(use-package mcp
  :if (package-installed-p 'mcp)
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem")))
             ("git" . (:command "uvx" :args ("mcp-server-git")))
             ("effect-mcp" . (:command "npx" :args ("-y" "effect-mcp@latest")))))
  :config
  (require 'mcp-hub)
  (when (package-installed-p 'gptel)
    (require 'gptel-integrations))
  :hook (after-init . mcp-hub-start-all-server))

(provide 'init-ai)
;;; init-ai.el ends here.
