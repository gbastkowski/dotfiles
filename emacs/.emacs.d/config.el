(defconst emacs-start-time (current-time))

(setq gc-cons-threshold   402653184
      gc-cons-percentage  0.6
      message-log-max     16384)

(defun gumacs/mac-p () (eq system-type 'darwin))
(defun gumacs/linux-p () (eq system-type 'gnu/linux))
(defun gumacs/mswindows-p () (eq system-type 'windows-nt))

(defun gumacs/window-system-mac-p ()
  ;; ns is returned instead of mac on Emacs 25+
  (memq (window-system) '(mac ns)))

(defun gumacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under the leader key.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(gumacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key gumacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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

(straight-use-package 'use-package)

(defvar gumacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "0") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "1") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "2") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "3") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "4") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "5") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "6") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "7") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "8") 'spacemacs-buffer/jump-to-number-startup-list-line)
    ;; (define-key map (kbd "9") 'spacemacs-buffer/jump-to-number-startup-list-line)

    ;; (define-key map [down-mouse-1] 'wid get-button-click)
    (define-key map (kbd "RET") 'gumacs-buffer/return)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)

    ;; (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for spacemacs-buffer mode.")

(define-derived-mode gumacs-buffer-mode special-mode "Gumacs buffer"
  "Gumacs major mode for startup screen."
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (page-break-lines-mode +1)
  (with-eval-after-load 'evil
    (progn
      (evil-set-initial-state 'gumacs-buffer-mode 'motion)
      (evil-make-overriding-map gumacs-buffer-mode-map 'motion)))
  (suppress-keymap gumacs-buffer-mode-map t)
  (set-keymap-parent gumacs-buffer-mode-map nil)
  (setq-local buffer-read-only t
              truncate-lines t))

(prefer-coding-system 'utf-8)

(use-package ucs-utils
  :straight (uu-patch :type git :host github :repo "rolandwalker/ucs-utils"))

(use-package list-utils
  :straight (lu-patch :type git :host github :repo "rolandwalker/list-utils"))

(use-package font-utils
  :straight (fu-patch :type git :host github :repo "rolandwalker/font-utils"))

(use-package unicode-fonts
  :straight (uf-patch :type git :host github :repo "rolandwalker/unicode-fonts")
  :init
  (progn
    (when (eq window-system 'ns)
      (setq unicode-fonts-skip-font-groups '(decorative low-quality-glyphs))))
:config
(unicode-fonts-setup))

(use-package persistent-soft
  :defer t)

(use-package ligature
  :straight (el-patch :type git :host github :repo "mickeynp/ligature.el"))

;; (use-package ace-link
;;   :commands spacemacs/ace-buffer-links
;;   :init
;;   (progn
;;     (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
;;     (with-eval-after-load 'info
;;       (define-key Info-mode-map "o" 'ace-link-info))
;;     (with-eval-after-load 'help-mode
;;       (define-key help-mode-map "o" 'ace-link-help))
;;     (with-eval-after-load 'woman
;;       (define-key woman-mode-map "o" 'link-hint-open-link))
;;     (with-eval-after-load 'eww
;;       (define-key eww-link-keymap "o" 'ace-link-eww)
;;       (define-key eww-mode-map "o" 'ace-link-eww))))

(use-package ace-window
  :straight t
  :defer t
  :init
  (progn
    ;; (spacemacs/set-leader-keys
      ;; "bD" 'spacemacs/ace-kill-this-buffer
      ;; FIXME: Needs new binding.
      ;; "wC" 'spacemacs/ace-center-window
      ;; "wD" 'spacemacs/ace-delete-window
      ;; "wM" 'ace-swap-window
      ;; "wW" 'ace-window)
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    )
  )

(use-package winum
  :ensure t)

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun gumacs/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun gumacs/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(use-package darcula-theme
  :ensure t
  :config
  (set-frame-font "MesloLGS NF")
  (load-theme 'darcula t))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

(use-package vim-powerline
  :straight (vp-patch :type git :host github :repo "milkypostman/powerline")
  :ensure t
  :hook (after-init . powerline-default-theme))

(use-package zoom-frm
  :commands (zoom-frm-unzoom
             zoom-frm-out
             zoom-frm-in)
  :init
  (progn
    (spacemacs|define-transient-state zoom-frm
      :title "Zoom Frame Transient State"
      :doc "
[_+_/_=_/_k_] zoom frame in   [_m_] max frame
[_-_/___/_j_] zoom frame out  [_f_] fullscreen
[_0_]^^^^     reset zoom      [_q_] quit"
      :bindings
      ("+" spacemacs/zoom-frm-in)
      ("=" spacemacs/zoom-frm-in)
      ("k" spacemacs/zoom-frm-in)
      ("-" spacemacs/zoom-frm-out)
      ("_" spacemacs/zoom-frm-out)
      ("j" spacemacs/zoom-frm-out)
      ("0" spacemacs/zoom-frm-unzoom)
      ("f" spacemacs/toggle-frame-fullscreen-non-native)
      ("m" spacemacs/toggle-maximize-frame)
      ("q" nil :exit t))
    (spacemacs/set-leader-keys "zf" 'spacemacs/zoom-frm-transient-state/body)

    ;; Font size, either with ctrl + mouse wheel
    (global-set-key (kbd "<C-wheel-up>") 'spacemacs/zoom-frm-in)
    (global-set-key (kbd "<C-wheel-down>") 'spacemacs/zoom-frm-out)))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

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

(package-install 'bind-map)
(require 'bind-map)
(bind-map gumacs-default-map
  :prefix-cmd gumacs-cmds
  :keys ("M-m")
  :evil-keys ("SPC")
  :override-minor-modes t
  :override-mode-name gumacs-leader-override-mode)

(package-install 'which-key)
(require 'which-key)
(which-key-mode)

(use-package helm
  :straight t
  :config
  (progn
    (use-package ace-jump-helm-line
      :defer (or idle-time t)
      :init
      (with-eval-after-load 'helm
        (define-key helm-map (kbd "C-q") 'ace-jump-helm-line)))
    (use-package helm-ag
      :straight t)
    (use-package helm-descbinds
      :straight t)
    (use-package helm-mode-manager
      :straight t)
    (use-package helm-org
      :straight t)
    (use-package helm-projectile
      :straight t)
    (use-package helm-swoop
      :straight t)
    (use-package helm-xref
      :straight t)
    (use-package imenu
      :straight t)
    (use-package persp-mode
      :straight t)
    (use-package popwin
      :straight t
      :init
      ;; (popwin-mode 1)
      )
    (use-package projectile
      :straight t)
    (helm-mode)
    (helm-descbinds-mode)
    (require 'helm-config)
    (setq helm-input-idle-delay                     0.01
          helm-reuse-last-window-split-state        t
          helm-always-two-windows                   t
          helm-split-window-inside-p                nil
          helm-commands-using-frame                 '(completion-at-point
                                                      helm-apropos
                                                      helm-eshell-prompts helm-imenu
                                                      helm-imenu-in-all-buffers)
          helm-actions-inherit-frame-settings       t
          helm-use-frame-when-more-than-two-windows t
          helm-use-frame-when-dedicated-window      t
          helm-frame-background-color               "DarkSlateGray"
          helm-show-action-window-other-window      'left
          helm-allow-mouse                          t
          helm-move-to-line-cycle-in-source         t
          helm-autoresize-max-height                80 ; it is %.
          helm-autoresize-min-height                20 ; it is %.
          helm-debug-root-directory                 "/Users/gunnar.bastkowski/tmp/helm-debug"
          helm-follow-mode-persistent               t
          helm-candidate-number-limit               500
          helm-visible-mark-prefix                  "✓")
    (set-face-foreground 'helm-mark-prefix "Gold1")
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)

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
      (simpler-helm-bookmark-keybindings))))

(use-package anzu
  :straight t)

(global-anzu-mode +1)

(use-package magit
  :straight t)

(defun gumacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun gumacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (kill-emacs))

(defun gumacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
    (make-frame-invisible nil 1))))

;; (gumacs/set-leader-keys
;; "qs" 'save-buffers-kill-emacs
;; "qq" 'gumacs/prompt-kill-emacs
;; "qQ" 'kill-emacs
;; "qf" 'gumacs/frame-killer)

(define-key  gumacs-default-map  (kbd "SPC")    (cons "M-x"                     'helm-M-x))

(setq gumacs-files-map (make-sparse-keymap))
(define-key  gumacs-default-map  "f"        (cons "Files"                    gumacs-files-map))
(define-key  gumacs-files-map    "f"        (cons "open file"               'helm-find-files))
(define-key  gumacs-files-map    "r"        (cons "recent files"            'helm-recentf))

(setq gumacs-buffers-map (make-sparse-keymap))
(define-key  gumacs-default-map  "b"        (cons "Buffers"                  gumacs-buffers-map))
(define-key  gumacs-buffers-map  "."        (cons "buffer transient state"  'spacemacs/buffer-transient-state/body))
(define-key  gumacs-buffers-map  "b"        (cons "list buffers"            'helm-mini))

(setq gumacs-help-map (make-sparse-keymap))
(define-key  gumacs-default-map  "h"        (cons "Help"                     gumacs-help-map))
(define-key  gumacs-help-map     "k"        (cons "show top level"          'which-key-show-top-level))
(define-key  gumacs-help-map     "RET"      (cons "helm-enable-minor-mode"  'helm-enable-minor-mode))
(define-key  gumacs-help-map     "<return>" (cons "helm-enable-minor-mode"  'helm-enable-minor-mode))

(setq gumacs-describe-map (make-sparse-keymap))
(define-key  gumacs-help-map     "d"        (cons "describe"                 gumacs-describe-map))
(define-key  gumacs-describe-map "k"        (cons "key"                     'describe-key))

(setq gumacs-quit-map (make-sparse-keymap))
(define-key  gumacs-default-map  "q"    (cons "Quit"                         gumacs-quit-map))
(define-key  gumacs-quit-map     "q"    (cons "prompt and quit"             'gumacs/prompt-kill-emacs))

(setq gumacs-toggles-map (make-sparse-keymap))
(define-key  gumacs-default-map  "t"    (cons "Toggles"                      gumacs-toggles-map))
(define-key  gumacs-toggles-map  "l"    (cons "truncate lines"              'toggle-truncate-lines))

(setq gumacs-windows-map (make-sparse-keymap))

;; from https://gist.github.com/3402786
(defun gumacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
            (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(define-key  gumacs-default-map  "w"    (cons "Windows"                        gumacs-windows-map))
(define-key  gumacs-windows-map  "d"    (cons "delete window"                 'delete-window))
(define-key  gumacs-windows-map  "m"    (cons "maximize buffer"               'gumacs/toggle-maximize-buffer))
(define-key  gumacs-windows-map  "w"    (cons "other window"                  'other-window))
(define-key  gumacs-windows-map  "W"    (cons "select window"                 'ace-window))
(define-key  gumacs-windows-map  "v"    (cons "split window right"            'split-window-right))
(define-key  gumacs-windows-map  "V"    (cons "split window right and focus"  'split-window-right-and-focus))
(define-key  gumacs-windows-map  "s"    (cons "split window below"            'split-window-below))
(define-key  gumacs-windows-map  "S"    (cons "split window below and focus"  'split-window-below-and-focus))
(define-key  gumacs-windows-map  "_"    (cons "maximize horizontally"         'gumacs/maximize-horizontally))
(define-key  gumacs-windows-map  "|"    (cons "split window right and focus"  'gumacs/maximize-vertically))
