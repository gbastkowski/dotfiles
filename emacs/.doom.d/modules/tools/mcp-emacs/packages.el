;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
;; plz: HTTP/SSE backend for the opencode client (soft dependency of mcp-emacs).
(package! plz)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :files ("elisp/*.el"))
  ;; Doom's `:pin' takes a commit hash only -- it abbreviates the value with
  ;; `substring', so a tag name errors out (doom-packages.el).
  ;;
  ;; v1.12.0
  :pin "3b5e25f4169c175f0cb0dd87a67465cc13eff302")
