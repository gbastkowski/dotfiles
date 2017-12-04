;; -*- mode: emacs-lisp -*-
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ansible
     asciidoc
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence ",,")
     c-c++
     chrome
     clojure
     colors
     common-lisp
     csv
     dash
     docker
     emacs-lisp
     emoji
     (erc             :variables
                      erc-enable-sasl-auth t
                      erc-server-list
                      '(("irc.freenode.net"
                         :port "6697"
                         :ssl t
                         :nick "gunbas")))
     evil-cleverparens
     ;; fasd
     git
     github
     go
     gnus
     graphviz
     groovy
     haskell
     helm
     html
     ibuffer
     java
     javascript
     (latex           :variables
                      latex-build-command "LatexMk"
                      latex-enable-auto-fill t
                      latex-enable-folding t)
     markdown
     (mu4e            :variables
                      mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
     (org             :variables
                      org-enable-bootstrap-support t
                      org-enable-github-support t
                      org-enable-reveal-js-support t
                      org-enable-org-journal-support t
                      org-journal-dir "~/org/journal/"
                      org-journal-file-format "%Y-%m-%d")
     (osx             :variables
                      osx-use-option-as-meta nil)
     nginx
     pandoc
     pdf-tools
     php
     plantuml
     puppet
     python
     (ranger          :variables
                      ranger-show-preview t)
     restclient
     (ruby            :variables
                      ruby-test-runner 'rspec)
     rust
     (scala           :variables
                      scala-enable-eldoc t
                      scala-auto-insert-asterisk-in-comments t
                      flycheck-stalastyle-jar "/usr/local/Cellar/scalastyle/0.8.0/libexec/scalastyle_2.11-0.8.0-batch.jar")
     semantic
     (shell           :variables
                      shell-default-shell 'multiterm
                      shell-default-height 30
                      shell-default-position 'bottom)
     shell-scripts
     slack
     speed-reading
     (spell-checking  :variables
                      spell-checking-enable-auto-dictionary t
                      spell-checking-enable-by-default nil)
     sql
     syntax-checking
     systemd
     terraform
     themes-megapack
     typescript
     (typography      :variables
                      typography-enable-typographic-editing nil)
     vagrant
     version-control
     windows-scripts
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     all-the-icons
     all-the-icons-dired
     dictcc
     editorconfig
     graphql-mode
     jenkins
     org-jira
     pretty-mode
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
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
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
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
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
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(tangotango
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
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("PragmataPro Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
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
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
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
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
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
   dotspacemacs-pretty-docs t
   ))

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

  (setenv "EDITOR" "emacsclient -c")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Fix MacOS key bindings
  (setq mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-right-command-modifier 'meta
        mac-option-modifier nil
        ns-function-modifier 'super)

  (spacemacs/toggle-typographic-substitutions-off)

  ;; System integration
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  (setq frame-title-format '(buffer-file-name "%f" "Emacs: %b"))

  ;; Readability
  (define-key global-map (kbd "C-+") 'hs-hide-all)
  (define-key global-map (kbd "C--") 'hs-toggle-hiding)
  (setq ns-auto-hide-menu-bar nil)
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
  (spacemacs/set-leader-keys "ab" 'calendar)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (setq org-catch-invisible-edits 'show)

  (setq jiralib-url "https://jira.numberfour.eu/")

  (setq org-bullets-bullet-list '("●" "◆" "▴" "▸"))

  ;; org-directory
  ;; org-default-notes-file
  (setq org-directory "~/org/")
  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "gtd.org"))
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (awk . t)
                                   (ditaa . t)
                                   (dot . t)
                                   (java . t)
                                   (plantuml . t)
                                   (ruby . t)
                                   (scala . t)))

  (setq org-plantuml-jar-path
        (expand-file-name "/usr/local/Cellar/plantuml/1.2017.13/libexec/plantuml.jar"))

  (setq org-capture-templates '(
                                ("i" "Inbox"          entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %^{Brief Description} %^g\n %?%i\n Added: %U\n")
                                ("t" "Todo"           entry (file+headline org-default-notes-file "Tasks")
                                 "* TODO %?\n %i\n %a")
                                ("r" "Reading List"   entry (file+headline org-default-notes-file "Reading List")
                                 "")
                                ("j" "Journal"        entry (file+datetree (concat org-directory "bookmarks.org"))
                                 "")
                                ("k" "Knowledge"      entry (file          (concat org-directory "notes.org"))
                                 "")
                                ("x" "org-protocol"   entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO Review %c\n%U\n%i\n Added: %U\n" :immediate-finish)))

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq diary-file     (concat org-directory "diary"))
  (setq timeclock-file (concat org-directory "timeclock"))

  (setq org-feed-alist '(
                         ("Slashdot"
                          "http://rss.slashdot.org/Slashdot/slashdot"
                          (concat org-directory "feeds.org")
                          "Slashdot Entries")))

  (setq org-mobile-directory "~/org/")

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
  (defun org-jenkins-open (path)
    "Visit the Jenkins project identified by PATH"
    (browse-url (concat "http://build-master.corp.numberfour.eu:8080/view/All/job/" path)))
  (org-add-link-type "jenkins" 'org-jenkins-open)
  ;; org n4github - custom link type to link to github repos
  (defun org-n4gh-open (path)
    "Visit the Jenkins project identified by PATH"
    (browse-url (concat "https://github.numberfour.eu/NumberFour/" path)))
  (org-add-link-type "n4gh" 'org-n4gh-open)


  ;; Jenkins
  (setq jenkins-api-token "c628a4d5f7afce3df56a40cba8974c54")
  (setq jenkins-url "http://cd1-jenkins.service.cd-dev.consul:8083")
  (setq jenkins-username "gbastkowski")
  (setq jenkins-viewname "allout-")
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

  ;; mu4e - Emacs as mail client

  ;;; (mu4e/mail-account-reset)

  (setq mu4e-maildir (expand-file-name "~/Maildir"))

  (setq mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder "/[Gmail].Sent Mail"
        mu4e-trash-folder "/[Gmail].Trash")

  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval nil
        mu4e-index-update-error-warning nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t)

  (setq user-mail-address "gunnar.bastkowski@numberfour.eu"
        user-full-name "Gunnar Bastkowski")

  (setq smtpmail-default-smtp-server "smtp.numberfour.eu"
        smtpmail-smtp-server "smtp.numberfour.eu"
        smtpmail-smtp-service 587)

  (slack-register-team
   :name "NumberFour"
   :default t
   :client-id "2556033269.226011525622"
   :client-secret "aad0458550c197db0b035f5a478d4415"
   :token "NMXTQugpcWMUCHhTzc6W1Iuw"
   :subscribed-channels '(general slackbot))

  (global-company-mode)

  ;; Language specific
  (setq markdown-command "/usr/local/bin/pandoc")
  (setq ensime-startup-snapshot-notification nil)
  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("def" .      #x2131)
             ("not" .      #x2757)
             ("in" .       #x2208)
             ("not in" .   #x2209)
             ("return" .   #x27fc)
             ("yield" .    #x27fb)
             ("for" .      #x2200)
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d)
             ;; Mypy
             ("Dict" .     #x1d507)
             ("List" .     #x2112)
             ("Tuple" .    #x2a02)
             ("Set" .      #x2126)
             ("Iterable" . #x1d50a)
             ("Any" .      #x2754)
             ("Union" .    #x22c3)))))

  ;; AUCTeX
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Stop creating backups and lock files
  (setq create-lockfiles nil
        backup-directory-alist '((".*" . "~/.Trash")))

  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(org-agenda-custom-commands
   (quote
    (("H" "Office and Home Lists"
      ((agenda "" nil)
       (tags-todo "@computer" nil)
       (tags-todo "@reading" nil)
       (tags-todo "@home" nil)
       (tags-todo "@office" nil))
      nil))))
 '(org-agenda-files (quote ("~/org/notes.org" "~/org/gtd.org")))
 '(org-agenda-include-diary t)
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 5)
     (org-agenda-files :tag . "project"))))
 '(org-refile-use-outline-path nil)
 '(package-selected-packages
   (quote
    (tss yaxception shm py-yapf org-repo-todo jade-mode helm-flyspell yapfify tide typescript-mode pyvenv pytest pyenv-mode py-isort plantuml-mode pip-requirements magit-gh-pulls live-py-mode hy-mode helm-pydoc graphviz-dot-mode github-search github-clone github-browse-file gist gh marshal logito pcache cython-mode company-anaconda anaconda-mode pythonic csv-mode sourcerer-theme slime-company insert-shebang hide-comnt helm-purpose window-purpose imenu-list minitest dictcc company-web ac-ispell typo helm-company helm-c-yasnippet web-completion-data company-tern dash-functional company-statistics company-shell company-emoji company-emacs-eclim company-cabal company-auctex common-lisp-snippets clojure-snippets auto-yasnippet auto-complete scalaz-unicode-input-method zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color web-mode web-beautify vagrant-tramp vagrant underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tern tao-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance stekene-theme srefactor sql-indent spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restclient rbenv ranger rake railscasts-theme purple-haze-theme puppet-mode pug-mode professional-theme planet-theme phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy pastels-on-dark-theme pandoc-mode ox-pandoc osx-trash osx-dictionary orgit organic-green-theme org-projectile org-present org org-pomodoro org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-http noflet noctilux-theme niflheim-theme naquadah-theme mwim mustang-theme multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e gntp monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow lush-theme livid-mode skewer-mode simple-httpd light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc jbeans-theme jazz-theme ir-black-theme intero inkpot-theme ibuffer-projectile htmlize hlint-refactor hindent heroku-theme hemisu-theme helm-hoogle helm-gitignore helm-dash helm-css-scss hc-zenburn-theme haskell-snippets haml-mode gruvbox-theme gruber-darker-theme groovy-mode grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck flatui-theme flatland-theme fish-mode firebelly-theme farmhouse-theme evil-magit magit magit-popup git-commit with-editor evil-cleverparens espresso-theme eshell-z eshell-prompt-extras esh-help ensime sbt-mode scala-mode emoji-cheat-sheet-plus emmet-mode editorconfig eclim drupal-mode php-mode dracula-theme django-theme diff-hl dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-ghci company-ghc ghc company haskell-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode chruby cherry-blossom-theme busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-dictionary auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme adoc-mode markup-faces ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(send-mail-function (quote sendmail-query-once)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

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
 '(evil-want-Y-yank-to-eol nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(org-agenda-custom-commands
   (quote
    (("H" "Office and Home Lists"
      ((agenda "" nil)
       (tags-todo "@computer" nil)
       (tags-todo "@reading" nil)
       (tags-todo "@home" nil)
       (tags-todo "@office" nil))
      nil))))
 '(org-agenda-files (quote ("~/org/notes.org" "~/org/gtd.org")))
 '(org-agenda-include-diary t)
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 5)
     (org-agenda-files :tag . "project"))))
 '(org-refile-use-outline-path nil)
 '(package-selected-packages
   (quote
    (powershell tss yaxception shm py-yapf org-repo-todo jade-mode helm-flyspell yapfify tide typescript-mode pyvenv pytest pyenv-mode py-isort plantuml-mode pip-requirements magit-gh-pulls live-py-mode hy-mode helm-pydoc graphviz-dot-mode github-search github-clone github-browse-file gist gh marshal logito pcache cython-mode company-anaconda anaconda-mode pythonic csv-mode sourcerer-theme slime-company insert-shebang hide-comnt helm-purpose window-purpose imenu-list minitest dictcc company-web ac-ispell typo helm-company helm-c-yasnippet web-completion-data company-tern dash-functional company-statistics company-shell company-emoji company-emacs-eclim company-cabal company-auctex common-lisp-snippets clojure-snippets auto-yasnippet auto-complete scalaz-unicode-input-method zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color web-mode web-beautify vagrant-tramp vagrant underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tern tao-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance stekene-theme srefactor sql-indent spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restclient rbenv ranger rake railscasts-theme purple-haze-theme puppet-mode pug-mode professional-theme planet-theme phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy pastels-on-dark-theme pandoc-mode ox-pandoc osx-trash osx-dictionary orgit organic-green-theme org-projectile org-present org org-pomodoro org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-http noflet noctilux-theme niflheim-theme naquadah-theme mwim mustang-theme multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e gntp monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow lush-theme livid-mode skewer-mode simple-httpd light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc jbeans-theme jazz-theme ir-black-theme intero inkpot-theme ibuffer-projectile htmlize hlint-refactor hindent heroku-theme hemisu-theme helm-hoogle helm-gitignore helm-dash helm-css-scss hc-zenburn-theme haskell-snippets haml-mode gruvbox-theme gruber-darker-theme groovy-mode grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck flatui-theme flatland-theme fish-mode firebelly-theme farmhouse-theme evil-magit magit magit-popup git-commit with-editor evil-cleverparens espresso-theme eshell-z eshell-prompt-extras esh-help ensime sbt-mode scala-mode emoji-cheat-sheet-plus emmet-mode editorconfig eclim drupal-mode php-mode dracula-theme django-theme diff-hl dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-ghci company-ghc ghc company haskell-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode chruby cherry-blossom-theme busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-dictionary auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme adoc-mode markup-faces ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(send-mail-function (quote sendmail-query-once)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
