;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :branch "main"
                              :files ("elisp/*.el"))
  ;; v1.9.0.  Doom's `:pin' takes a commit hash only -- it abbreviates the
  ;; value with `substring', so a tag name errors out (doom-packages.el).
  :pin "a1914e2d7043fc5f382ab6118065e02de6e441d6")
