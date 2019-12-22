;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ansible
     asciidoc
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     c-c++
     chrome
     clojure
     (colors :variables
             colors-enable-nyan-cat-progress-bar nil)
     csv
     dash
     docker
     (elfeed :variables
             rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")
             elfeed-enable-web-interface nil)
     emacs-lisp
     emoji
     (erc :variables
          erc-enable-sasl-auth t
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "gunbas")))
     fasd
     git
     github
     gnus
     go
     graphviz
     groovy
     gunnar
     haskell
     (helm :variables
           helm-position 'bottom)
     html
     ibuffer
     imenu-list
     (java :variables
           java-backend 'lsp)
     javascript
     json
     kotlin
     (latex :variables
            latex-build-command "LatexMk"
            latex-enable-auto-fill t
            latex-enable-folding t)
     lsp
     major-modes
     markdown
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
           mu4e-use-maildirs-extension t
           mu4e-enable-async-operations t)
     multiple-cursors
     (org  :variables
           org-enable-bootstrap-support t
           org-enable-github-support t
           org-enable-reveal-js-support t
           org-enable-org-journal-support t
           org-journal-dir "~/org/journal/"
           org-journal-file-format "%Y-%m-%d")
     nginx
     notmuch
     (osx :variables
          osx-option-as 'meta
          osx-right-option-as 'meta)
     pandoc
     pass
     pdf
     php
     (plantuml :variables
               plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
               org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
     puppet
     python
     (ranger :variables
             ranger-show-preview t)
     ;; (rebox :variables
     ;;        rebox-enable-in-text-mode t)
     (restclient :variables
                 restclient-use-org t)
     (ruby :variables
           ruby-test-runner 'rspec)
     rust
     (scala-lsp :variables
            scala-auto-insert-asterisk-in-comments t
            scala-enable-eldoc t
            scala-use-unicode-arrows nil)
     selectric
     semantic
     (shell :variables
            shell-default-shell 'multiterm
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     spacemacs-purpose
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     spell-checking-enable-by-default nil)
     (sql :variables sql-capitalize-keywords t)
     (syntax-checking :variables)
     systemd
     terraform
     themes-megapack
     treemacs
     typescript
     (typography :variables
                 typography-enable-typographic-editing nil)
     unicode-fonts
     vagrant
     version-control
     vimscript
     windows-scripts
     xkcd
     xclipboard
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     all-the-icons
     all-the-icons-dired
     dictcc
     editorconfig
     graphql-mode
     jenkins
     org-jira
     (scalaz-unicode-input-method :location
                                  (recipe
                                  :fetcher github
                                  :repo "folone/emacs-scalaz-unicode-input-method"))
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 2

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(
                                (agenda    . 20)
                                (recents   . 25)
                                (projects  . 10)
                                (bookmarks . nil)
                                )
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gunnar
                         tangotango
                         sanityinc-tomorrow-night
                         alect-black
                         flatland
                         sanityinc-tomorrow-bright
                         seti
                         monokai
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(vim-powerline :separator bar :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("PragmataProMono Nerd Font"
                               :size 14
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
                                            ("org"   . "orgmode.org/elpa/")
                                            ("gnu"   . "elpa.gnu.org/packages/")))

  (setq-default git-enable-magit-svn-plugin t)
  (setq dotspacemacs-elpa-https nil)
  (setq ispell-program-name "aspell")

  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

  (setenv "EDITOR" "emacsclient -c")

  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (spacemacs/toggle-typographic-substitutions-off)
  (setq-default dotspacemacs-smartparens-strict-mode t)

  (setq blink-cursor-blinks 0
        cursor-type 'bar)
  (blink-cursor-mode 1)

  (setq-default fill-column 120)
  (setq fci-rule-color "#555555"
        fci-rule-width 1)

  ;; Umlauts
  ;; (define-key key-translation-map (kbd "M-a") (kbd "ä"))
  ;; (define-key key-translation-map (kbd "M-A") (kbd "Ä"))
  ;; (define-key key-translation-map (kbd "M-o") (kbd "ö"))
  ;; (define-key key-translation-map (kbd "M-O") (kbd "Ö"))
  ;; (define-key key-translation-map (kbd "M-u") (kbd "ü"))
  ;; (define-key key-translation-map (kbd "M-U") (kbd "Ü"))
  ;; (define-key key-translation-map (kbd "M-s") (kbd "ß"))

  ;; Projects
  (setq projectile-project-search-path '("~/git/smarttra/" "~/git/gbastkowski/"))
  
  ;; Cool folds
  (define-key global-map (kbd "H-.") 'evil-toggle-fold)

  ;; Readability
  (global-prettify-symbols-mode t)
  (spacemacs/set-leader-keys "tP" 'global-prettify-symbols-mode)

  ;; Magit
  (global-git-commit-mode t)
  (define-key global-map (kbd "C-x g") 'magit-status)
  (eval-after-load "git-commit-mode"
    '(remove-hook 'git-commit-mode-hook 'flyspell-mode))

  ;; Avy
  (define-key global-map (kbd "C-:") 'avy-goto-char)
  (spacemacs/set-leader-keys "oj" 'avy-goto-char)
  (spacemacs/set-leader-keys "ok" 'avy-goto-char-2)

  ;; Multiple Edits
  (spacemacs/set-leader-keys "om" 'mc/edit-lines)
  (spacemacs/set-leader-keys "or" 'replace-rectangle)

  ;; Org/Calendar and Productivity
  (spacemacs/set-leader-keys "aw" 'eww)
  (spacemacs/set-leader-keys "ab" 'calendar)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (setq org-catch-invisible-edits 'show)

  (setq auth-sources '(password-store))
  (setq jiralib-url "https://smarttradede.atlassian.net")
  (setq jiralib-token
        (let ((found (nth 0 (auth-source-search :max 1
                                                :host (url-host (url-generic-parse-url jiralib-url))
                                                :port 80
                                                :require '(:user :secret)
                                                :create nil)))
              user secret)
          (when found
            (setq user (plist-get found :user)
                  secret
                  (let ((sec (plist-get found :secret)))
                    (if (functionp sec)
                        (funcall sec)
                      sec)))
            `("Authorization" . , (format "Basic %s" (base64-encode-string (concat user ":" secret)))))))

  (spacemacs/set-leader-keys "ajpg" 'org-jira-get-projects)
  (spacemacs/set-leader-keys "ajib" 'org-jira-browse-issue)
  (spacemacs/set-leader-keys "ajig" 'org-jira-get-issues)
  (spacemacs/set-leader-keys "ajih" 'org-jira-get-issues-headonly)
  (spacemacs/set-leader-keys "ajiu" 'org-jira-update-issue)
  (spacemacs/set-leader-keys "ajiw" 'org-jira-progress-issue)
  (spacemacs/set-leader-keys "ajin" 'org-jira-progress-issue-next)
  (spacemacs/set-leader-keys "ajia" 'org-jira-assign-issue)
  (spacemacs/set-leader-keys "ajia" 'org-jira-assign-issue)
  (spacemacs/set-leader-keys "ajir" 'org-jira-refresh-issue)
  (spacemacs/set-leader-keys "ajiR" 'org-jira-refresh-issues-in-buffer)
  (spacemacs/set-leader-keys "ajic" 'org-jira-create-issue)
  (spacemacs/set-leader-keys "ajik" 'org-jira-copy-current-issue-key)
  (spacemacs/set-leader-keys "ajsc" 'org-jira-create-subtask)
  (spacemacs/set-leader-keys "ajsg" 'org-jira-get-subtasks)
  (spacemacs/set-leader-keys "ajcu" 'org-jira-update-comment)
  (spacemacs/set-leader-keys "ajwu" 'org-jira-update-worklogs-from-org-clocks)
  (spacemacs/set-leader-keys "ajtj" 'org-jira-todo-to-jira)
  (spacemacs/set-leader-keys "ajif" 'org-jira-get-issues-by-fixversion)


  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1)
  (setq calendar-mark-holidays-flag 1)
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-constant-face))
  (setq calendar-intermonth-header
        (propertize "KW"
                    'font-lock-face 'font-lock-keyword-face))


  ;; org Jenkins - custom link type to link to jenkins builds
  ;; (defun org-jenkins-open (path)
  ;; "Visit the Jenkins project identified by PATH"
  ;; (browse-url (concat "http://build-master.corp.numberfour.eu:8080/view/All/job/" path)))
  ;; (org-add-link-type "jenkins" 'org-jenkins-open)
  ;; org n4github - custom link type to link to github repos
  ;; (defun org-n4gh-open (path)
  ;; "Visit the Jenkins project identified by PATH"
  ;; (browse-url (concat "https://github.numberfour.eu/NumberFour/" path)))
  ;; (org-add-link-type "n4gh" 'org-n4gh-open)

  ;; Jenkins
  (setq jenkins-api-token "c628a4d5f7afce3df56a40cba8974c54")
  (setq jenkins-url "https://jenkins.smarttra.de")
  (setq jenkins-username "gunnar")
  (setq jenkins-viewname "allout-")
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

  ;; Cool helpers
  (defun find-stderr ()
    "Finds ~/Downloads/stderr"
    (interactive)
    (let ((value (find-file-noselect "~/Downloads/stderr" nil nil nil)))
      (if (listp value)
          (mapcar 'switch-to-buffer (nreverse value))
        (switch-to-buffer value))))

  (defun find-stdout ()
    "Finds ~/Downloads/stdout"
    (interactive)
    (let ((value (find-file-noselect "~/Downloads/stdout" nil nil nil)))
      (if (listp value)
          (mapcar 'switch-to-buffer (nreverse value))
        (switch-to-buffer value))))

  (spacemacs/set-leader-keys "ye" 'find-stderr)
  (spacemacs/set-leader-keys "yo" 'find-stdout)

  ;; gnus - Emacs as mail client
  (setq gnus-secondary-select-methods
        '(
          (nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          ))
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com")
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmaiul]/Sent Mail")
  (setq gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com"))))
  (setq nnml-directory "~/.mail/gmail")
  (setq message-directory "~/.mail/gmail")

  ;; mu4e - Emacs as mail client
  (setq mu4e-maildir "~/.mail/smarttra"
        user-full-name "Gunnar Bastkowski"
        user-mail-address "gunnar.bastkowski@ttmzero.com"
        mu4e-inbox-folder "/Inbox"
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "mbsync smarttra"
        mu4e-update-interval 120 ; seconds
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-enable-notifications t
        mu4e-enable-mode-line t)

  (with-eval-after-load 'mu4e-alert
    (mu4e-alert-set-default-style 'notifications))


  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-maildir-shortcuts
        '(("/Inbox" . ?i)))

  ;; (setq smtpmail-default-smtp-server "smtp.numberfour.eu"
  ;;               smtpmail-smtp-server "smtp.numberfour.eu"
  ;;               smtpmail-smtp-service 587)

  ;; Stop creating backups and lock files
  (setq create-lockfiles nil
        backup-directory-alist '((".*" . "~/.Trash")))

  ;; Language specific
  ;;; AUCTeX
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;;; Emacs Listp
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)

  ;;; Markdown
  (setq markdown-command "/usr/bin/pandoc")

  ;;; plantuml
  (setq plantuml-output-type "png")

  ;;; Scala
  (setq-default flycheck-scalastylerc "/Users/gunnar.bastkowski/.scalastyle_config.xml")

  ;;; XML
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil-blank-string))
  ;;; HTML
  (setq web-mode-markup-indent-offset 2)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (treemacs-resize-icons 16)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(provide '.spacemacs)
;;; .spacemacs ends here

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6beb95ba786e22f0e1e24816c95ac45da8d6dd886242eb8017e2e0374b45fe06" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(lsp-file-watch-threshold 2000)
 '(org-agenda-files (quote ("~/org/gtd.org" "~/org/bookmarks.org")))
 '(package-selected-packages
   (quote
    (ob-ammonite elfeed-web elfeed-org elfeed-goodies ace-jump-mode elfeed writeroom-mode treemacs-evil tide robe pretty-mode pandoc-mode orgit org-download magit-svn lsp-ui kaolin-themes helm-xref gruvbox-theme forge evil-nerd-commenter evil-magit dumb-jump doom-themes doom-modeline docker color-theme-sanityinc-tomorrow cider sesman clojure-mode browse-at-remote aggressive-indent ace-link ac-php-core counsel swiper ivy lsp-mode magit transient lv flycheck pythonic helm all-the-icons treemacs ace-window org-plus-contrib hydra zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color xcscope ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights visual-fill-column vi-tilde-fringe vagrant-tramp vagrant uuidgen use-package underwater-theme ujelly-theme typo typescript-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile toxi-theme toml-mode toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit tablist systemd symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance srefactor sql-indent spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shrink-path shell-pop seti-theme selectric-mode seeing-is-believing scss-mode scalaz-unicode-input-method sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer queue pyvenv pytest pyenv-mode py-isort purple-haze-theme puppet-mode pug-mode professional-theme prettier-js powershell popwin plantuml-mode planet-theme pippel pipenv pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme pfuture persp-mode password-generator paradox ox-twbs ox-reveal ox-pandoc ox-gfm overseer osx-trash osx-dictionary organic-green-theme org-projectile org-present org-pomodoro org-mime org-journal org-jira org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http noflet noctilux-theme nginx-mode naquadah-theme nameless mvn mustang-theme multi-term mu4e-maildirs-extension mu4e-alert move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme meghanada maven-test-mode material-theme markdown-toc majapahit-theme magithub magit-gitflow madhat2r-theme macrostep lush-theme lsp-java lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme launchctl json-navigator json-mode js2-refactor js-doc jinja2-mode jenkins jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-pass helm-org-rifle helm-mu helm-mode-manager helm-make helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruber-darker-theme groovy-mode groovy-imports graphviz-dot-mode graphql-mode grandshell-theme gradle-mode gotham-theme google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gmail-message-mode gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme fuzzy font-lock+ flyspell-correct-helm flymd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav eldoc-eval editorconfig edit-server drupal-mode dracula-theme dotenv-mode dockerfile-mode docker-tramp django-theme disaster diminish diff-hl dictcc dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode cquery counsel-projectile company-web company-terraform company-tern company-statistics company-shell company-rtags company-restclient company-quickhelp company-php company-lsp company-go company-ghci company-emoji company-emacs-eclim company-cabal company-c-headers company-auctex company-ansible company-anaconda column-enforce-mode color-theme-sanityinc-solarized color-identifiers-mode cmm-mode clues-theme closql clojure-snippets clean-aindent-mode clang-format cider-eval-sexp-fu chruby cherry-blossom-theme centered-cursor-mode ccls cargo busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme all-the-icons-dired alect-themes afternoon-theme adoc-mode ace-jump-helm-line ac-ispell)))
 '(plantuml-default-exec-mode (quote jar))
 '(sql-connection-alist
   (quote
    (("postgres-smarttrade-localhost"
      (sql-product
       (quote postgres))
      (sql-user "smarttrade")
      (sql-database "smarttrade")
      (sql-server "localhost")))))
 '(treemacs-width 50)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
