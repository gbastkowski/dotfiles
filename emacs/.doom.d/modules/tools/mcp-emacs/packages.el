;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :branch "main"
                              :files ("elisp/*.el"))
  :pin "011974c6d57538bdfbcf301d7bdc6c626ab4118b")
