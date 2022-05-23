(setq helm-packages
      '(
        ace-jump-helm-line
        auto-highlight-symbol
        bookmark
        helm
        helm-ag
        helm-descbinds
        helm-flx
        (helm-ls-git :toggle (configuration-layer/layer-used-p 'git))
        helm-make
        helm-mode-manager
        helm-org
        helm-projectile
        helm-swoop
        helm-themes
        (helm-spacemacs-help :location local)
        (helm-spacemacs-faq :location local)
        helm-xref
        imenu
        persp-mode
        popwin
        projectile
        ))

(use-package helm-descbinds)
(use-package helm
  :straight t
  :config
  (progn
    (helm-mode)
    ;; (spacemacs|hide-lighter helm-mode)
    ;; (advice-add 'helm-grep-save-results-1 :after 'spacemacs//gne-init-helm-grep)
    ;; helm-locate uses es (from everything on windows which doesn't like fuzzy)
    ;; (helm-locate-set-command)
    ;; (setq helm-locate-fuzzy-match (and (bound-and-true-p helm-use-fuzzy)
    ;;                                    (string-match "locate" helm-locate-command)))
    ;; (setq helm-boring-buffer-regexp-list
    ;;       (append helm-boring-buffer-regexp-list
    ;;               spacemacs-useless-buffers-regexp))
    ;; (setq helm-white-buffer-regexp-list
    ;;       (append helm-white-buffer-regexp-list
    ;;               spacemacs-useful-buffers-regexp))
    ;; use helm to switch last(/previous) visited buffers with C(-S)-tab
    (define-key helm-map (kbd "<C-tab>") 'helm-follow-action-forward)
    (define-key helm-map (kbd "<C-iso-lefttab>") 'helm-follow-action-backward)
    ;; alter helm-bookmark key bindings to be simpler
    (defun simpler-helm-bookmark-keybindings ()
      (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
      (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
      (define-key helm-bookmark-map
        (kbd "C-f") 'helm-bookmark-toggle-filename)
      (define-key helm-bookmark-map
        (kbd "S-<return>") 'helm-bookmark-run-jump-other-window)
      (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
    (with-eval-after-load 'helm-bookmark
      (simpler-helm-bookmark-keybindings))
    ;; (when (configuration-layer/package-used-p 'winum)
    ;;   (define-key helm-buffer-map
    ;;     (kbd "RET") 'spacemacs/helm-find-buffers-windows)
    ;;   (define-key helm-generic-files-map
    ;;     (kbd "RET") 'spacemacs/helm-find-files-windows)
    ;;   (define-key helm-find-files-map
    ;;     (kbd "RET") 'spacemacs/helm-find-files-windows))
    ))
