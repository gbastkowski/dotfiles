;;; ui/window-transient/config.el -*- lexical-binding: t; -*-

;; Spacemacs' window transient state, ported to Doom.  `SPC w .' opens a
;; sticky hydra: every head keeps the map alive, so a run of moves, swaps
;; and resizes costs one prefix instead of one per command.  `q' or ESC
;; leaves it.
;;
;; The key layout follows Spacemacs so muscle memory carries over:
;; hjkl move, HJKL swap, / and - split, +-<> resize, u undo the layout.
;;
;; `defhydra' is a macro, so the hydra cannot be defined until the package
;; is loaded.  Rather than pull hydra in at startup, the leader key calls a
;; small stub that requires hydra, defines the real hydra once, and hands
;; over to it -- so the cost is paid on first use.

(use-package! hydra
  :defer t
  :config
  (defhydra +window-transient (:hint nil)
    "
^Move^            ^Swap^            ^Split^          ^Resize^        ^Layout^
_h_/_j_/_k_/_l_ direction  _H_/_J_/_K_/_L_ direction  _/_ vertical     _+_/_-_ height  _=_ balance
_w_ ace-jump      _r_ rotate        _s_ horizontal   _<_/_>_ width   _m_ maximize
_p_ previous      _o_ other frame   _d_ close        ^ ^             _u_/_U_ undo/redo
^ ^               ^ ^               _c_ close others ^ ^             _q_ quit
"
    ;; Move
    ("h" evil-window-left)
    ("j" evil-window-down)
    ("k" evil-window-up)
    ("l" evil-window-right)
    ("p" evil-window-mru)
    ("w" ace-window :exit t)
    ;; Swap
    ("H" +evil/window-move-left)
    ("J" +evil/window-move-down)
    ("K" +evil/window-move-up)
    ("L" +evil/window-move-right)
    ("r" evil-window-rotate-downwards)
    ("o" other-frame)
    ;; Split
    ("/" evil-window-vsplit)
    ("s" evil-window-split)
    ("d" evil-window-delete)
    ("c" delete-other-windows)
    ;; Resize
    ("+" evil-window-increase-height)
    ("-" evil-window-decrease-height)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ;; Layout
    ("=" balance-windows)
    ("m" doom/window-maximize-buffer)
    ("u" winner-undo)
    ("U" winner-redo)
    ("q" nil :exit t)
    ("<escape>" nil :exit t)))

(defun +window-transient/start ()
  "Enter the window transient state, loading hydra on first use."
  (interactive)
  (require 'hydra)
  (+window-transient/body))

(map! :leader :desc "Window transient state" "w." #'+window-transient/start)
