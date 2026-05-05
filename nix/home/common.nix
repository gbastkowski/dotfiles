{ inputs, ... }:
{
  programs.home-manager.enable = true;

  home.stateVersion = "25.11";

  home.file.".p10k.zsh".source = ../../zsh/.p10k.zsh;
  home.file.".local/bin/checkmail.sh"        = { source = ../../bin/checkmail.sh;        executable = true; };
  home.file.".local/bin/emacs-capture.sh"    = { source = ../../bin/emacs-capture.sh;    executable = true; };
  home.file.".local/bin/emacs-upgrade.sh"    = { source = ../../bin/emacs-upgrade.sh;    executable = true; };
  home.file.".local/bin/eos-capture-video.sh"= { source = ../../bin/eos-capture-video.sh;executable = true; };
  home.file.".local/bin/git-ensure-remotes.sh"={ source = ../../bin/git-ensure-remotes.sh;executable = true; };
  home.file.".local/bin/install-xmlls.sh"    = { source = ../../bin/install-xmlls.sh;    executable = true; };
  home.file.".local/bin/linux.include.sh"    = { source = ../../bin/linux.include.sh;    executable = true; };
  home.file.".local/bin/macos.include.sh"    = { source = ../../bin/macos.include.sh;    executable = true; };
  home.file.".local/bin/metals-emacs"        = { source = ../../bin/metals-emacs;        executable = true; };
  home.file.".local/bin/ollama-coder.sh"     = { source = ../../bin/ollama-coder.sh;     executable = true; };
  home.file.".local/bin/ollama-install.sh"   = { source = ../../bin/ollama-install.sh;   executable = true; };
  home.file.".local/bin/ostype.sh"           = { source = ../../bin/ostype.sh;           executable = true; };
  home.file.".local/bin/release.sh"          = { source = ../../bin/release.sh;          executable = true; };
  home.file.".local/bin/system-upgrade.sh"   = { source = ../../bin/system-upgrade.sh;   executable = true; };
  home.file.".local/bin/termux.include.sh"   = { source = ../../bin/termux.include.sh;   executable = true; };

  home.file.".doom.d".source = ../../emacs/.doom.d;
  home.file.".emacs-profile".source = ../../emacs/.emacs-profile;
  home.file.".emacs-profiles.el".source = ../../emacs/.emacs-profiles.el;

  home.file.".wgetrc".source = ../../wget/.wgetrc;

  home.file.".android-env".source = ../../home/.android-env;
  home.file.".ideavimrc".source = ../../idea/.ideavimrc;
  home.file.".uniteai.yml".source = ../../home/.uniteai.yml;
  home.file.".gnupg/gpg-agent.conf".source = ../../home/.gnupg/gpg-agent.conf;
  home.file.".gnupg/pinentry-wrapper" = {
    source = ../../home/.gnupg/pinentry-wrapper;
    executable = true;
  };

  programs.zsh = {
    enable = true;

    autosuggestion.enable = false;
    syntaxHighlighting.enable = false;

    oh-my-zsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [
        "common-aliases"
        "direnv"
        "docker"
        "docker-compose"
        "dotenv"
        "extract"
        "fasd"
        "gem"
        "git"
        "git-lfs"
        "gitignore"
        "gpg-agent"
        "helm"
        "history"
        "history-substring-search"
        "kubectl"
        "mvn"
        "nvm"
        "pass"
        "sbt"
        "scala"
        "sdk"
        "screen"
        "virtualenv"
        "zsh-vi-mode"
      ];
      extraConfig = ''
        ZSH_DOTENV_FILE=.env.local
        ZSH_DISABLE_COMPFIX=true
        DISABLE_AUTO_UPDATE="true"
        COMPLETION_WAITING_DOTS="true"
      '';
    };

    sessionVariables = {
      TERM = "xterm-256color";
      DEFAULT_USER = "gunnar";
      GTAGSLABEL = "pygments";
      EDITOR = "emacsclient -t";
      OPENCODE_EXPERIMENTAL = "true";
      NODE_REPL_HISTORY = "~/.node_history";
      NODE_REPL_HISTORY_SIZE = "32768";
      NODE_REPL_MODE = "sloppy";
      PYTHONIOENCODING = "UTF-8";
      HISTSIZE = "32768";
      HISTFILESIZE = "32768";
      HISTCONTROL = "ignoreboth";
      LANG = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      MANPAGER = "less -X";
      SDKMAN_DIR = "$HOME/.sdkman";
      NVM_DIR = "$HOME/.nvm";
    };

    initContent = ''
      # Return early for IntelliJ environment reader
      if [[ -n "$INTELLIJ_ENVIRONMENT_READER" ]]; then
        return
      fi

      # p10k instant prompt
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      # PATH helpers
      _path_contains() {
        dir=$1
        [[ -z ''${dir:-} ]] && return 1
        case ":''${PATH:-}:" in *:"$dir":*) return 0 ;; esac
        return 1
      }
      path_prepend() {
        dir=$1
        [[ -z ''${dir:-} ]] && return
        if ! _path_contains "$dir"; then
          PATH="$dir''${PATH:+:$PATH}"
          export PATH
        fi
      }
      path_append() {
        dir=$1
        [[ -z ''${dir:-} ]] && return
        if ! _path_contains "$dir"; then
          PATH="''${PATH:+$PATH:}$dir"
          export PATH
        fi
      }

      # JAVA_HOME
      ensure_java_home() {
        if [[ -n ''${SDKMAN_DIR:-} && -d ''${SDKMAN_DIR}/candidates/java/current ]]; then
          export JAVA_HOME="''${SDKMAN_DIR}/candidates/java/current"
        elif command -v /usr/libexec/java_home >/dev/null 2>&1; then
          local j
          j=$(/usr/libexec/java_home 2>/dev/null || true)
          [[ -n $j ]] && export JAVA_HOME="$j"
        else
          local jp
          jp="$(command -v java 2>/dev/null || true)"
          [[ -n $jp && $jp == */bin/java ]] && export JAVA_HOME="''${jp%/bin/java}"
        fi
      }
      ensure_java_home

      # GPG
      if command -v gpg-connect-agent >/dev/null 2>&1; then
        export GPG_TTY="$(tty)"
        gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1 || true
      fi

      # kitty socket
      if [[ -n "''${KITTY_WINDOW_ID:-}" ]]; then
        export KITTY_LISTEN_ON="''${KITTY_LISTEN_ON:-unix:/tmp/kitty-$USER}"
      fi

      # Private/work config
      [[ -f $HOME/.ista_rc ]] && source "$HOME/.ista_rc"
      [[ -f $HOME/.private ]] && source "$HOME/.private"

      # p10k config
      [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

      # PATH additions
      [[ -d $HOME/.bin ]] && path_prepend "$HOME/.bin"
      [[ -d $HOME/go/bin ]] && path_prepend "$HOME/go/bin"
      path_prepend "./node_modules/.bin"
      [[ -d $HOME/.emacs.doom/bin ]] && path_append "$HOME/.emacs.doom/bin"
      [[ -d $HOME/.rbenv/bin ]] && path_prepend "$HOME/.rbenv/bin"
      [[ -d $HOME/.local/bin ]] && path_append "$HOME/.local/bin"

      # rbenv
      eval "$(rbenv init - zsh)"

      # sdkman
      [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"

      # nvm
      if [[ "$(uname)" == "Linux" ]]; then
        [[ -s "/usr/share/nvm/init-nvm.sh" ]] && source "/usr/share/nvm/init-nvm.sh"
      fi

      # Dart completions (Linux only)
      [[ -f /home/gunnar/.dart-cli-completion/zsh-config.zsh ]] && source /home/gunnar/.dart-cli-completion/zsh-config.zsh || true

      # Arch-specific
      case "$(uname -a)" in
        *arch*)
          [[ -n "$SSH_CONNECTION" ]] && systemctl --user start inhibit-suspend.service
          ;;
      esac

      # cargo
      [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

      # local bin helper
      [[ -f "$HOME/.local/bin/ensure-local-bin-in-path" ]] && source "$HOME/.local/bin/ensure-local-bin-in-path"

      # Shell functions
      mcd() { mkdir "$1" && cd "$1"; }
      pman() {
        for name in "$@"; do
          man -t "$name" | pstopdf -i
          mv -f stdin.pdf ~/Documents/man-pages/"$name".pdf
          open ~/Documents/man-pages/"$name".pdf
        done
      }

      unalias run-help 2>/dev/null || true
      autoload run-help
    '';
  };
  home.file.".oh-my-zsh/custom/themes/powerlevel10k".source = inputs.powerlevel10k;
  home.file.".oh-my-zsh/custom/plugins/zsh-vi-mode".source = inputs.zsh-vi-mode;
  home.file.".sbt/1.0/build.sbt".source = ../../sbt/.sbt/1.0/build.sbt;
  home.file.".sbt/1.0/plugins/plugins.sbt".source = ../../sbt/.sbt/1.0/plugins/plugins.sbt;
  home.file.".sbt/1.0/credentials.sbt".source = ../../sbt/.sbt/1.0/credentials.sbt;
  home.file.".sbt/repositories".source = ../../sbt/.sbt/repositories;

  programs.tmux = {
    enable = true;
    prefix = "C-a";
    baseIndex = 1;
    escapeTime = 1;
    historyLimit = 10000;
    mouse = true;
    keyMode = "vi";
    terminal = "xterm-256color";

    extraConfig = ''
      bind C-a send-prefix
      unbind C-b

      bind-key r source-file ~/.tmux.conf \; display-message "tmux config reloaded."

      setw -g pane-base-index 1
      set -g allow-passthrough on

      setw -g monitor-activity on
      set -g visual-activity on

      bind Escape copy-mode
      unbind p
      bind p paste-buffer

      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection
      bind-key -T copy-mode-vi 'Space' send -X halfpage-down
      bind-key -T copy-mode-vi 'BSpace' send -X halfpage-up

      bind C-c run "tmux safe-buffer - | xclip -i sel clipboard"
      bind C-v run "tmux set-buffer \"$(xclip -o -sel clipboard)\"; tmux paste-buffer"

      bind / split-pane -h
      bind - split-pane -v
      unbind '"'
      unbind %

      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      bind -r C-h select-window -t :-
      bind -r C-l select-window -t :+

      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5
    '';
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user.name = "Gunnar Bastkowski";
      user.email = "gunnar.bastkowski@ista.com";
      user.signingkey = "D97D98E973249CDC50C02CB2C2D116E278472281";
      pull.rebase = true;
      merge.conflictstyle = "diff3";
      push.followTags = true;
      github.user = "gbastkowski";
      advice.detachedHead = false;
      init.defaultBranch = "main";
    };
    ignores = [
      "*.elc"
      "auto-save-list"
      "tramp"
      ".#*"
      ".bloop/"
      ".classpath"
      "/GPATH"
      "/GRTAGS"
      "/GTAGS"
      ".idea/"
      "/keybase/"
      "*.log"
      ".metals/"
      ".project"
      "/.scala_history"
      ".settings/"

      # Org-mode
      ".org-id-locations"
      "*_archive"

      # flymake-mode
      "*_flymake.*"

      # eshell files
      "/eshell/history"
      "/eshell/lastdir"

      # elpa packages
      "/elpa/"

      # reftex files
      "*.rel"

      "*.7z"
      "*.dmg"
      "*.gz"
      "*.iso"
      "*.jar"
      "!gradle-wrapper.jar"
      "*.rar"
      "*.tar"

      "*.log"
      "*.sqlite"

      # OS generated files
      ".DS_Store"
      ".DS_Store?"
      "._*"
      ".Spotlight-V100"
      ".Trashes"
      "ehthumbs.db"
      "Thumbs.db"

      ".*.swp"

      "TODOs.org"
      "/local.sbt"
      "/project/metals.sbt"
      "**/project/metals.sbt"
      "/.bsp/"
      "/TAGS"
      "*.~undo-tree~"
      "/.dir-locals.el"

      # Local overrides
      "/*.local.org"
      "/*.local.md"

      # Claude runtime state
      "**/.claude/settings.local.json"
      "/.claude/"
      "/claude/debug/"
      "/claude/file-history/"
      "/claude/history.jsonl"
      "/claude/projects/"
      "/claude/shell-snapshots/"
      "/claude/statsig/"
      "/claude/todos/"
    ];
  };
}
