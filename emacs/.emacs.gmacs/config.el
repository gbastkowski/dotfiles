(cond ((eq system-type 'gnu/linux)
       (setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh"))
      (t
       (setenv "SSH_AUTH_SOCK" "/Users/gunnar.bastkowski/.gnupg/S.gpg-agent.ssh")))

(setq auth-sources '(password-store))

(use-package persp-mode
  :config
  (persp-mode 1))

(desktop-save-mode 1)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(set-face-attribute 'default nil :font "IosevkaNerdFont" :height 130)
(gmacs/load-config "ligatures.el")

(use-package minimap)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq custom-theme-directory (expand-file-name "themes/" user-emacs-directory))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'gunnar t)
  (use-package all-the-icons)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                shell-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emojify
  :straight t
  :hook (doom-first-buffer . global-emojify-mode)
  :config
  (setq emojify-styles
        (delq
         nil (list (if (modulep! +ascii) 'ascii)
                   (if (modulep! +github) 'github)
                   (if (modulep! +unicode) 'unicode))))

  (emojify-set-emoji-styles emojify-styles))

(add-hook 'org-mode-hook #'org-indent-mode)

(setq org-indent-indentation-per-level 2)
(setq org-edit-src-content-indentation 0)

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
	org-hide-leading-stars nil
	org-superstar-headline-bullets-list '("◉" "○" "⬥" "⬦" "▸" "▹")
	org-superstar-todo-bullet-alist
	'(("TODO" . 9744)
	  ("[ ]"  . 9744)
	  ("DONE" . 9745)
	  ("[X]"  . 9745))))

(use-package org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚑" "⬆" "■")))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.24)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-on-exact-match nil)
  (corfu-popupinfo-mode t)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match corfu-quit-at-boundary)
  (global-corfu-modes '((not erc-mode
			    circe-mode
			    help-mode
			    gud-mode
			    vterm-mode)
		       t))
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu)

(use-package corfu-terminal)

(use-package yasnippet-capf)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)) ; Enable orderless matching
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; (use-package vertico-directory
;;   :after vertico
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :init
  (recentf-mode))

(use-package consult-dir)

(use-package consult-flycheck)

(use-package embark-consult)

(use-package embark)

(use-package wgrep)

(use-package nerd-icons-completion)

(use-package vertico-posframe)

(use-package consult-yasnippet)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration))

(use-package typescript-mode
  :mode "\\.\s\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package command-log-mode)

(with-eval-after-load 'org
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit))
