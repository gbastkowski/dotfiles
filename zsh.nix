{ inputs, lib, ... }:
{
  home.file.".p10k.zsh".source = ./zsh/.p10k.zsh;
  home.file.".oh-my-zsh/custom/themes/powerlevel10k".source = inputs.powerlevel10k;
  home.file.".oh-my-zsh/custom/plugins/zsh-vi-mode".source = inputs.zsh-vi-mode;

  programs.zsh = {
    enable = true;

    autosuggestion.enable = false;
    syntaxHighlighting.enable = false;

    oh-my-zsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [
        "common-aliases"
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
        "pass"
        "sbt"
        "scala"
        "screen"
        "virtualenv"
        "zsh-vi-mode"
      ];
      extraConfig = ''
        ZSH_DOTENV_FILE=.env.local
        ZSH_DISABLE_COMPFIX=true
        DISABLE_AUTO_UPDATE="true"
        COMPLETION_WAITING_DOTS="true"
        ZSH_CUSTOM="$HOME/.oh-my-zsh/custom"
      '';
    };

    sessionVariables = {
      TERM = "xterm-256color";
      DEFAULT_USER = "gunnar";
      GTAGSLABEL = "pygments";
      OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
      OPENCODE_EXPERIMENTAL_PLAN_MODE = "true";
      OPENCODE_EXPERIMENTAL_OXFMT = "true";
      OPENCODE_EXPERIMENTAL_FILEWATCHER = "true";
      OPENCODE_EXPERIMENTAL_ICON_DISCOVERY = "true";
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
    };

    envExtra = ''
      # SSH agent socket via gpg-agent (all shells: login, interactive, non-interactive)
      if command -v gpgconf >/dev/null 2>&1; then
        sock="$(gpgconf --list-dirs agent-ssh-socket 2>/dev/null)"
        [[ -n "$sock" ]] && export SSH_AUTH_SOCK="$sock"
      fi
    '';

    initContent = lib.mkMerge [
     (lib.mkOrder 500 ''
      # Return early for IntelliJ environment reader
      if [[ -n "$INTELLIJ_ENVIRONMENT_READER" ]]; then
        return
      fi

      # p10k instant prompt — must be before oh-my-zsh
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      # oh-my-zsh "screen" plugin hostname override: map deess1mac* → ista
      _GET_HOST='h=$(hostname -s 2>/dev/null || hostname); case "$h" in deess1mac*) h=ista ;; esac; echo "$h"'

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
        if command -v /usr/libexec/java_home >/dev/null 2>&1; then
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

      # PATH additions
      [[ -d $HOME/.bin ]] && path_prepend "$HOME/.bin"
      [[ -d $HO../bin ]] && path_prepend "$HO../bin"
      path_prepend "./node_modules/.bin"
      [[ -d $HOME/.local/bin ]] && path_append "$HOME/.local/bin"
    '')
    (lib.mkAfter ''
      # Private/work config
      [[ -f $HOME/.ista_rc ]] && source "$HOME/.ista_rc"
      [[ -f $HOME/.private ]] && source "$HOME/.private"

      # p10k config
      [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

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
    '')
    ];
  };
}
