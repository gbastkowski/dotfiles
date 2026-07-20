;; -*- no-byte-compile: t; -*-
;;; tools/mcp-emacs/packages.el

(package! web-server)
(package! mcp-emacs  :recipe (:host github :repo "gbastkowski/mcp-emacs"
                              :branch "main"
                              :files ("elisp/*.el"))
  :pin "4d4405f1b0c5a0c26675ac2f3959fdbf4ff9ccb0")
