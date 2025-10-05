;;; init-ai.el --- Configuration for gptel           -*- lexical-binding: t; -*-

;;; Commentary

;;; Code

(use-package gptel
  :if (package-installed-p 'gptel)
  :custom
  (gptel-tools '())
  (gptel-model 'claude-sonnet-4-5-20250929)
  ;; (gptel-backend (gptel-make-anthropic "Claude"
  ;;                  :stream t
  ;;                  :key #'init-get-anthropic-key))
  :preface

  (defun init-get-anthropic-key ()
    (password-store-get "anthropic-api-key"))

  :config

  (gptel-make-preset 'coder
                     :description "A preset for general programming tasks"
                     :backend "Claude"
                     :model 'claude-sonnet-4-5-20250929
                     :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."))

(use-package mcp
  :if (package-installed-p 'mcp)
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(provide 'init-ai)
;;; init-ai.el ends here.
