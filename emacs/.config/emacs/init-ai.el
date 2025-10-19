;;; init-ai.el --- Configuration for gptel           -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

(use-package gptel
    :if (package-installed-p 'gptel)
    :preface

    (defun init-get-anthropic-key ()
      (password-store-get "anthropic-api-key"))

    :hook
    ((gptel-post-response-functions . gptel-end-of-response))

    :config
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t
                          :models '(claude-sonnet-4-5-20250929 claude-opus-4-1-20250805)
                          :key #'init-get-anthropic-key)
          gptel-model 'claude-sonnet-4-5-20250929
          gptel-default-mode 'org-mode
          gptel-temperature 0.0
          gptel-cache t
          gptel-use-tools t)

    (require 'init-ai-tools
             (expand-file-name "init-ai-tools.el" user-emacs-directory))

    (gptel-make-preset 'prompt-generator
      :description "An assistant for writting system prompts"
      :system 'prompt-generator
      :backend "Claude"
      :model 'claude-sonnet-4-5-20250929
      :temperature 0.0
      :tools '("read_buffer"
               "create_buffer"
               "pop_to_buffer"
               "insert_into_buffer"
               "open_file_buffer"))

    (gptel-make-preset 'mermaid
      :description "An assistant for generating mermaid documents"
      :system 'mermaid-diagram-assistant
      :backend "Claude"
      :model 'claude-sonnet-4-5-20250929
      :temperature 0.0
      :tools '("read_buffer"
               "create_buffer"
               "pop_to_buffer"
               "insert_into_buffer"
               "open_file_buffer"))

    (gptel-make-preset 'coding
      :description "A general programming preset"
      :system "You are an expert AI programming assistant."
      :backend "Claude"
      :model 'claude-sonnet-4-5-20250929
      :temperature 0.0
      :tools '("read_buffer"
               "create_buffer"
               "pop_to_buffer"
               "insert_into_buffer"
               "open_file_buffer"
               "create_patch_buffer"
               "get_project_buffers"
               "get_project_files"
               "get_buffer_diagnostics"
               "get_buffer_diagnostics_by_line_number"))

    (gptel-make-preset 'effect-backend
      :parents '(coding)
      :system 'effect-backend
      :tools '(:append ())))

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
    :custom
    (mcp-hub-servers
     `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/jake")))
       ("git" . (:command "uvx" :args ("mcp-server-git")))
       ("effect-mcp" . (:command "npx" :args ("-y" "effect-mcp@latest")))
       ("mermaid" . (:command "npx" :args ("-y" "@peng-shawn/mermaid-mcp-server")))))
    :config
    (require 'mcp-hub)
    (when (package-installed-p 'gptel)
      (require 'gptel-integrations))
    :hook ((after-init . mcp-hub-start-all-server)))

(provide 'init-ai)
;;; init-ai.el ends here.
