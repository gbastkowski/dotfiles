;;; tools/mcp-emacs/config.el -*- lexical-binding: t; -*-

;; MCP server that runs inside this Emacs session and speaks the Model
;; Context Protocol over HTTP, so AI agents reach the live buffers,
;; windows, and Org state directly (no emacsclient round-trip per call).
;; Point an MCP client at http://localhost:8765/mcp.

(use-package! mcp-emacs-server
  :defer t
  :init
  (add-hook 'emacs-startup-hook #'mcp-emacs-server-ensure))
