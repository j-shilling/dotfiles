;;; init-ai.el --- Configuration for gptel           -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

(use-package gptel
  :if (package-installed-p 'gptel)
  :preface

  (defun init-get-anthropic-key ()
    (password-store-get "anthropic-api-key"))

  :hook
  ((gptel-post-response-functions . gptel-end-of-response)
   (gptel-post-stream-hook . gptel-auto-scroll)
   (gptel-mode-hook . gptel-highlight-mode))

  :config

  (require 'init-ai-tools
           (expand-file-name "init-ai-tools.el" user-emacs-directory))

  (defconst init-ai--claude
    (gptel-make-anthropic "Claude"
      :stream t
      :models '(claude-sonnet-4-5-20250929
                claude-opus-4-6)
      :key #'init-get-anthropic-key))
  (defconst init-ai--copilot
    (gptel-make-gh-copilot "Copilot"
      :models '(gpt-5.2)))

  (setq gptel-backend init-ai--copilot
        gptel-model 'gpt-5.2
        gptel-default-mode 'org-mode
        gptel-temperature 0.0
        gptel-cache t
        gptel-use-tools t
        gptel-include-tool-results t
        gptel-tools (nconc (gptel-get-tool "buffers")
                           (gptel-get-tool "filesystem"))))

(use-package mcp
  :if (package-installed-p 'mcp)
  :after gptel
  :custom
  (mcp-hub-servers
   `(("playwright" . (:command "npx" :args ("-y" "@playwright/mcp@latest")))
     ("mermaid" . (:command "npx" :args ("-y" "@peng-shawn/mermaid-mcp-server")))
     ("a11y" . (:command "npx" :args ("-y" "a11y-mcp")))
     ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp")))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("aws" . (:command "uvx" :args ("mcp-proxy-for-aws@latest" "https://aws-mcp.us-east-1.api.aws/mcp")))
     ("vitest" . (:command "npx" :args ("-y" "@madrus/vitest-mcp-server@latest")))
     ("terraform" . (:command "docker" :args ("run" "-i" "--rm" "hashicorp/terraform-mcp-server:0.4.0")))
     ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "HOME"))))))
  :config
  (require 'mcp-hub)
  (when (package-installed-p 'gptel)
    (require 'gptel-integrations)))

(provide 'init-ai)
;;; init-ai.el ends here.
