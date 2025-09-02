;; -*- no-byte-compile: t; -*-
;;; tools/claude-code/packages.el

(package! eat :recipe (:host github :repo "jamescherti/emacs-eat" :files ("*.el" ("term" "term/*.el") "*.texi"
                                                                           "*.ti" ("terminfo/e" "terminfo/e/*")
                                                                           ("terminfo/65" "terminfo/65/*")
                                                                           ("integration" "integration/*")
                                                                           (:exclude ".dir-locals.el" "*-tests.el"))))
(package! claude-code :recipe (:host github :repo "stevemolitor/claude-code.el"))