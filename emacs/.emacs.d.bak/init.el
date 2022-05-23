;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))

(setq gc-cons-threshold   402653184
      gc-cons-percentage  0.6
      message-log-max     16384)

(defun gumacs/load-files (files)
  (dolist (file files)
    (load (concat (file-name-directory load-file-name) file)
          nil
          (not init-file-debug))))

(defun gumacs/mac-p () (eq system-type 'darwin))
(defun gumacs/linux-p () (eq system-type 'gnu/linux))
(defun gumacs/mswindows-p () (eq system-type 'windows-nt))

(defun gumacs/window-system-mac-p ()
  ;; ns is returned instead of mac on Emacs 25+
  (memq (window-system) '(mac ns)))

(package-install 'bind-map)

(defun gumacs/init-bind-map ()
  (require 'bind-map)
  (bind-map gumacs-default-map
    :prefix-cmd gumacs-cmds
    :keys ("M-m")
    :evil-keys ("SPC")
    :override-minor-modes t
    :override-mode-name gumacs-leader-override-mode))

(defun gumacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under the leader key.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key gumacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defun gumacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-some-buffers nil t)
  (kill-emacs))

(defun gumacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (kill-emacs))

(defun gumacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

;; (require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package))

(require 'subr-x nil 'noerror)

(prefer-coding-system 'utf-8)

(gumacs/init-bind-map)

(gumacs/set-leader-keys
  ;; "qs" 'spacemacs/save-buffers-kill-emacs
  "qq" 'gumacs/prompt-kill-emacs
  "qQ" 'gumacs/kill-emacs
  "qf" 'gumacs/frame-killer)

(use-package evil
  :ensure t
  :config

  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s s" 'swiper
      "d x w" 'delete-trailing-whitespace))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (use-package evil-org
    :ensure t
    :config
    (evil-org-set-key-theme
	   '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode))))

  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme)))

(use-package helm
  :straight t
  :config
  (progn
    (use-package helm-ag)
    (use-package helm-descbinds)
    (use-package helm-org)
    (use-package helm-projectile)
    (use-package helm-swoop)
    (use-package helm-xref)
    (use-package imenu)
    (use-package projectile)
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

(use-package magit)
