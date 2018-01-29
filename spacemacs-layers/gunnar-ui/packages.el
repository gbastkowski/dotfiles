;; added to `gunnar-ui-packages'. Then, for each package PACKAGE:
;;   function `gunnar-ui/init-PACKAGE' to load and initialize the package.
;;   define the functions `gunnar-ui/pre-init-PACKAGE' and/or
;;   `gunnar-ui/post-init-PACKAGE' to customize the package as it is loaded.

(defconst gunnar-ui-packages
  '(pretty-mode))

(defun gunnar-ui/init-pretty-mode ()
  (use-package pretty-mode
    :defer t
    :init
    (with-eval-after-load 'pretty-mode
      ((global-pretty-mode t)))))

; if you want to set it only for a specific mode
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)
;;; packages.el ends here
