;; -*- no-byte-compile: t; -*-
;;; tools/claude-code/packages.el

(package! eat :recipe (:host github :repo "jamescherti/emacs-eat" :files ("*.el" ("term" "term/*.el") "*.texi"
                                                                           "*.ti" ("terminfo/e" "terminfo/e/*")
                                                                           ("terminfo/65" "terminfo/65/*")
                                                                           ("integration" "integration/*")
                                                                           (:exclude ".dir-locals.el" "*-tests.el"))))
(package! claude-code     :recipe (:host github :repo "stevemolitor/claude-code.el"))
(package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
(package! monet           :recipe (:host github :repo "stevemolitor/monet" :files ("monet.el")))
(package! websocket       :recipe (:host github :repo "ahyatt/emacs-websocket"))
