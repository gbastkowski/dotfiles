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
   '(python
     pass
     ansible
     asciidoc
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-idle-delay 0.0
                      auto-completion-minimum-prefix-length 1)
     bibtex
     c-c++
     chrome
     clojure
     (colors :variables
             colors-enable-nyan-cat-progress-bar nil)
     csv
     dash
     docker
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
     floobits
     git
     gnus
     go
     graphviz
     (groovy :variables
             groovy-backend 'lsp
             groovy-lsp-jar-path "/usr/share/java/groovy-language-server/groovy-language-server-all.jar")
     gunnar
     haskell
     (helm :variables
           helm-position 'bottom
           helm-use-fuzzy 'source)
     html
     ibuffer
     imenu-list
     (java :variables
           java-backend 'lsp)
     javascript
     json
     kotlin
     (latex :variables
            latex-backend 'lsp
            latex-build-command "LatexMk"
            latex-build-engine 'xetex
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-enable-magic t)
     (lsp :variables
          lsp-headerline-breadcrumb-enable nil
          lsp-headerline-breadcrumb-segments '(project file symbols)
          lsp-modeline-code-actions-enable t
          lsp-modeline-code-actions-segments '(count icon)
          lsp-modeline-diagnostics-enable t
          lsp-modeline-diagnostics-scope :project
          lsp-lens-enable t
          lsp-navigation 'both
          lsp-ui-doc-enable t
          lsp-ui-doc-include-signature nil
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-symbol nil
          lsp-use-lsp-ui t
          )
     major-modes
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; (mu4e :variables
     ;;       mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
     ;;       mu4e-use-maildirs-extension t
     ;;       mu4e-enable-async-operations t)
     multiple-cursors
     nginx
     node
     (org  :variables
           org-enable-appear-support t
           org-enable-asciidoc-support t
           org-enable-bootstrap-support t
           org-enable-epub-support t
           org-enable-github-support t
           org-enable-jira-support t
           jiralib-url "https://jira.mobimeo.com"
           org-enable-notifications t
           org-enable-reveal-js-support t
           org-enable-org-brain-support t
           org-enable-org-journal-support t
           org-enable-sticky-header t
           org-journal-dir "~/org/journal/"
           org-journal-file-format "%Y-%m-%d"
           org-start-notification-daemon-on-startup t)
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
     (ranger :variables
             ranger-show-preview t)
     ;; (rebox :variables
     ;;        rebox-enable-in-text-mode t)
     (restclient :variables
                 restclient-use-org t)
     (ruby :variables
           ruby-test-runner 'rspec)
     rust
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-sbt-window-position nil
            scala-enable-gtags t)
     selectric
     semantic
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-height 30
            shell-default-position 'bottom
            shell-enable-smart-eshell t)
     shell-scripts
     slack
     spacemacs-editing
     spacemacs-purpose
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     spell-checking-enable-by-default nil)
     (sql :variables
          sql-capitalize-keywords nil
          sql-backend 'lsp)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     systemd
     terraform
     themes-megapack
     treemacs
     typescript
     (typography :variables
                 typography-enable-typographic-editing nil)
     ;; (unicode-fonts :variables
     ;;                unicode-fonts-force-multi-color-on-mac t
     ;;                unicode-fonts-enable-ligatures t)
     vagrant
     ;; version-control
     vimscript
     windows-scripts
     xkcd
     xclipboard
     (yaml :variables yaml-enable-lsp t))

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     all-the-icons
     all-the-icons-dired
     atomic-chrome
     dictcc
     editorconfig
     ;; exec-path-from-shell
     graphql-mode
     jenkins
     org-jira
     (scalaz-unicode-input-method :location
                                  (recipe
                                  :fetcher github
                                  :repo "folone/emacs-scalaz-unicode-input-method"))
     smithy-mode)

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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

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

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

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
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '(
                                (agenda             . 20)
                                (recents-by-project . (5 . 7))
                                (recents            . 25)
                                (projects           . 10)
                                (bookmarks          . nil))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(darcula
                         gunnar
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

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Iosevka"       ;;"MesloLGS NF"
                               :size 14.0
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   dotspacemacs-auto-generate-layout-names t

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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
   dotspacemacs-line-numbers 't

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
                                            ("org"   . "orgmode.org/elpa/")
                                            ("gnu"   . "elpa.gnu.org/packages/")))

  (setq dotspacemacs-elpa-https nil)
  (setq ispell-program-name "aspell")

  ;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

  (setenv "EDITOR" "emacsclient -c")

  (add-to-list 'exec-path "/usr/local/bin")

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives))

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

  (atomic-chrome-start-server)

  (spacemacs/toggle-typographic-substitutions-off)

  ;; (ligature-set-ligatures
  ;;  't
  ;;  '("<==>" "<!--" "~~>" "***" "||>" ":::" "::"
  ;;    "===" "==>" "=>>" "=<<" "=/=" "!==" "!!"
  ;;    ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
  ;;    "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
  ;;    "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>"
  ;;    "#_(" "..<" "..." "+++" "/==" "///" "_|_" "&&" "^=" "~="
  ;;    "~>" "**" "*>" "*/" "||" "|=" "|>" "|-"
  ;;    "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
  ;;    ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
  ;;    "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
  ;;    "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." "..." ".?" "+>" "++" "?:"
  ;;    "?=" "?." "??" "/*" "/=" "</" "/>" "//" "__"
  ;;    "//"))

  (setq blink-cursor-blinks 0
        cursor-type 'bar)
  (blink-cursor-mode 1)
  (setq shell-default-shell 'vterm)

  (setq-default fill-column 120)
  (setq fci-rule-color "#555555"
        fci-rule-width 1)

  ;; (setq-default dotspacemacs-smartparens-strict-mode t)

  (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system
   (if (eq system-type 'windows-nt)
       'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
     'utf-8))
  (prefer-coding-system 'utf-8)

  ;; Umlauts
  (define-key key-translation-map (kbd "M-a") (kbd "ä"))
  (define-key key-translation-map (kbd "M-A") (kbd "Ä"))
  (define-key key-translation-map (kbd "M-o") (kbd "ö"))
  (define-key key-translation-map (kbd "M-O") (kbd "Ö"))
  (define-key key-translation-map (kbd "M-u") (kbd "ü"))
  (define-key key-translation-map (kbd "M-U") (kbd "Ü"))
  (define-key key-translation-map (kbd "M-s") (kbd "ß"))

  ;; Projects
  (setq projectile-project-search-path '("~/git/mobimeo/" "~/git/gbastkowski/"))

  ;; Readability
  (global-prettify-symbols-mode t)
  (spacemacs/set-leader-keys "tP" 'global-prettify-symbols-mode)

  ;; Magit
  ;; (global-git-commit-mode t)
  ;; (define-key global-map (kbd "C-x g") 'magit-status)
  ;; (eval-after-load "git-commit-mode"
  ;;   '(remove-hook 'git-commit-mode-hook 'flyspell-mode))

  ;; Avy
  (define-key global-map (kbd "C-:") 'avy-goto-char)
  (spacemacs/set-leader-keys "oj" 'avy-goto-char)
  (spacemacs/set-leader-keys "ok" 'avy-goto-char-2)

  ;; LSP
  (spacemacs/set-leader-keys "od" 'lsp-ui-doc-show)
  (spacemacs/set-leader-keys "oi" 'helm-semantic-or-imenu)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-treemacs-sync-mode t)

  ;; Multiple Edits
  (spacemacs/set-leader-keys "om" 'mc/edit-lines)
  (spacemacs/set-leader-keys "or" 'replace-rectangle)

  ;; Org/Calendar and Productivity
  (spacemacs/set-leader-keys "aw" 'eww)
  (spacemacs/set-leader-keys "ab" 'calendar)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (setq org-catch-invisible-edits 'show)

  (setq bibtex-completion-bibliography '("~/Documents/references.bib")
        bibtex-completion-library-path "~/Documents/"
        bibtex-completion-notes-path "~/Documents/notes.org")

  (setq auth-sources '(password-store))
  ;; (slack-register-team
  ;;  :name "mobimeo"
  ;;  :default t
  ;;  :client-id "gunnar.bastkowski@mobimeo.com"
  ;;  :token (password-store-get "slack/token")
  ;;  :subscribed-channels '(general devops))

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


  ;; Stop creating backups and lock files
  (setq create-lockfiles nil
        backup-directory-alist '((".*" . "~/.Trash")))

  ;; Language specific
  ;;; AUCTeX
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;;; Emacs Lisp

  ;;; Markdown
  ;; (setq markdown-command "/usr/bin/pandoc")

  ;;; plantuml
  (setq plantuml-output-type "png")

  ;;; Scala
  (setq-default flycheck-scalastylerc "/Users/gunnar.bastkowski/.scalastylerc.xml")

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

  (defun log-file-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.log\\'" buffer-file-name))
      auto-revert-tail-mode))

  (add-hook 'find-file-hook 'log-file-mode)

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
   '("6beb95ba786e22f0e1e24816c95ac45da8d6dd886242eb8017e2e0374b45fe06" default))
 '(evil-want-Y-yank-to-eol nil)
 '(json-reformat:indent-width 2)
 '(lsp-file-watch-threshold 2000)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-headerline-breadcrumb-segments '(project path-up-to-project symbols) t)
 '(lsp-metals-bloop-sbt-already-installed t)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-use-childframe nil)
 '(lsp-ui-doc-use-webkit t)
 '(org-agenda-files '("~/org/gtd.org" "~/org/bookmarks.org"))
 '(org-pomodoro-finished-sound
   "/Users/gunnar.bastkowski/Sounds/mixkit-correct-answer-reward-952.wav")
 '(org-pomodoro-start-sound
   "/Users/gunnar.bastkowski/Sounds/mixkit-positive-notification-951.wav")
 '(package-selected-packages
   '(blacken code-cells anaconda-mode counsel-gtags ggtags helm-cscope helm-gtags epc ctable concurrent lsp-pyright lsp-python-ms nose load-env-vars poetry pydoc pylookup sphinx-doc atomic-chrome restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well dap-mode bui tree-mode ob-ammonite elfeed-web elfeed-org elfeed-goodies ace-jump-mode elfeed writeroom-mode treemacs-evil tide robe pretty-mode pandoc-mode orgit org-download magit-svn lsp-ui kaolin-themes helm-xref gruvbox-theme forge evil-nerd-commenter evil-magit dumb-jump doom-themes doom-modeline docker color-theme-sanityinc-tomorrow cider sesman clojure-mode browse-at-remote aggressive-indent ace-link ac-php-core counsel swiper ivy lsp-mode magit transient lv flycheck pythonic helm all-the-icons treemacs ace-window org-plus-contrib hydra zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color xcscope ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights visual-fill-column vi-tilde-fringe vagrant-tramp vagrant uuidgen use-package underwater-theme ujelly-theme typo typescript-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile toxi-theme toml-mode toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit tablist systemd symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance srefactor sql-indent spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shrink-path shell-pop seti-theme selectric-mode seeing-is-believing scss-mode scalaz-unicode-input-method sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer queue pyvenv pytest pyenv-mode py-isort purple-haze-theme puppet-mode pug-mode professional-theme prettier-js powershell popwin plantuml-mode planet-theme pippel pipenv pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme pfuture persp-mode password-generator paradox ox-twbs ox-reveal ox-pandoc ox-gfm overseer osx-trash osx-dictionary organic-green-theme org-projectile org-present org-pomodoro org-mime org-journal org-jira org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noflet noctilux-theme nginx-mode naquadah-theme nameless mvn mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme meghanada maven-test-mode material-theme markdown-toc majapahit-theme magithub magit-gitflow madhat2r-theme macrostep lush-theme lsp-java lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme launchctl json-navigator json-mode js2-refactor js-doc jinja2-mode jenkins jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-pass helm-org-rifle helm-mu helm-mode-manager helm-make helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruber-darker-theme groovy-mode groovy-imports graphviz-dot-mode graphql-mode grandshell-theme gradle-mode gotham-theme google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gmail-message-mode gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme fuzzy font-lock+ flyspell-correct-helm flymd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav eldoc-eval editorconfig edit-server drupal-mode dracula-theme dotenv-mode dockerfile-mode docker-tramp django-theme disaster diminish diff-hl dictcc dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode cquery counsel-projectile company-web company-terraform company-tern company-statistics company-shell company-rtags company-quickhelp company-php company-lsp company-go company-ghci company-emoji company-emacs-eclim company-cabal company-c-headers company-auctex company-ansible company-anaconda column-enforce-mode color-theme-sanityinc-solarized color-identifiers-mode cmm-mode clues-theme closql clojure-snippets clean-aindent-mode clang-format cider-eval-sexp-fu chruby cherry-blossom-theme centered-cursor-mode ccls cargo busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme all-the-icons-dired alect-themes afternoon-theme adoc-mode ace-jump-helm-line ac-ispell))
 '(plantuml-default-exec-mode 'jar)
 '(sql-connection-alist
   '(("postgres-smarttrade-localhost"
      (sql-product 'postgres)
      (sql-user "smarttrade")
      (sql-database "smarttrade")
      (sql-server "localhost"))))
 '(treemacs-width 50)
 '(truncate-partial-width-windows t)
 '(vc-follow-symlinks t)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "green1" :foreground "#222222"))))
 '(font-lock-builtin-face ((t (:foreground "DarkOrange3" :weight bold))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(web-mode-html-tag-face ((t (:foreground "gold1")))))
)
