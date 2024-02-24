;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup gunnar-theme nil
  "Options for the `gunnar' theme."
  :group 'doom-themes)

(defcustom gunnar-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'gunnar-theme
  :type 'boolean)

(defcustom gunnar-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'gunnar-theme
  :type 'boolean)

(defcustom gunnar-comment-bg gunnar-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their legibility."
  :group 'gunnar-theme
  :type 'boolean)

(defcustom gunnar-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'gunnar-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme gunnar
  "A dark theme inspired by IntelliJ's darcula."

  ;; name        default        256           16
  ((bg         '("#282828"      "black"       "black"         ))
   (fg         '("#bbc2cf"      "#bfbfbf"     "brightwhite"   ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#21242b"      "black"       "black"         ))
   (fg-alt     '("#5B6268"      "#2d2d2d"     "white"         ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1B2229"      "black"       "black"         ))
   (base1      '("#1c1f24"      "#1e1e1e"     "brightblack"   ))
   (base2      '("#202328"      "#2e2e2e"     "brightblack"   ))
   (base3      '("#23272e"      "#262626"     "brightblack"   ))
   (base4      '("#3f444a"      "#3f3f3f"     "brightblack"   ))
   (base5      '("#5B6268"      "#525252"     "brightblack"   ))
   (base6      '("#73797e"      "#6b6b6b"     "brightblack"   ))
   (base7      '("#9ca0a4"      "#979797"     "brightblack"   ))
   (base8      '("#dfdfdf"      "#dfdfdf"     "white"         ))

   (grey       base4)
   (red        '("#ff6c6b"      "#ff6655"     "red"           ))
   (orange     '("#da8548"      "#dd8844"     "brightred"     ))
   (green      '("#98be65"      "#99bb66"     "green"         ))
   (teal       '("#4db5bd"      "#44b9b1"     "brightgreen"   ))
   (yellow     '("#ECBE7B"      "#ECBE7B"     "yellow"        ))
   (blue       '("#51afef"      "#51afef"     "brightblue"    ))
   (dark-blue  '("#2257A0"      "#2257A0"     "blue"          ))
   (magenta    '("#c678dd"      "#c678dd"     "brightmagenta" ))
   (orange     '("#cd6600"      "#cd6600"     "DarkOrange3"   ))
   (violet     '("#a9a1e1"      "#a9a1e1"     "magenta"       ))
   (cyan       '("#46D9FF"      "#46D9FF"     "brightcyan"    ))
   (dark-cyan  '("#5699AF"      "#5699AF"     "cyan"          ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       (if gunnar-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if gunnar-brighter-comments dark-cyan base5) 0.25))
   (constants      fg)
   (functions      (doom-lighten orange 0.7))
   (keywords       orange)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten orange 0.7))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if gunnar-brighter-modeline
                                 (doom-darken green 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if gunnar-brighter-modeline
                                 (doom-darken green 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when gunnar-padded-modeline
      (if (integerp gunnar-padded-modeline) gunnar-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if gunnar-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if gunnar-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if gunnar-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; org-mode
   (outline-1 :foreground green :weight 'bold)
   (outline-2 :foreground fg)
   (outline-3 :foreground fg :slant 'italic)
   (org-todo :foreground orange)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; gunnar-theme.el ends here
