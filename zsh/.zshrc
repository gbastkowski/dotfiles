# Return early when shell is loaded by IntelliJ, to prevent errors with interactive shell features
if [[ -n "$INTELLIJ_ENVIRONMENT_READER" ]]; then
    return
fi

# Load shared exports first so interactive shells inherit common env vars.
if [[ -r "$HOME/.exports" ]]; then
  source "$HOME/.exports"
fi

# Guarantee JAVA_HOME available for zsh sessions.
if ! typeset -f ensure_java_home >/dev/null 2>&1; then
  ensure_java_home() {
    if [[ -n ${SDKMAN_DIR:-} && -d ${SDKMAN_DIR}/candidates/java/current ]]; then
      export JAVA_HOME="${SDKMAN_DIR}/candidates/java/current"
      return
    fi

    if command -v /usr/libexec/java_home >/dev/null 2>&1; then
      local detected
      detected=$(/usr/libexec/java_home 2>/dev/null || true)
      if [[ -n $detected ]]; then
        export JAVA_HOME="$detected"
        return
      fi
    fi

    local java_path
    java_path="$(command -v java 2>/dev/null || true)"
    if [[ -n $java_path ]]; then
      local java_bin_dir
      java_bin_dir="$(dirname "$java_path")"
      if [[ $java_bin_dir == */bin ]]; then
        export JAVA_HOME="${java_bin_dir%/bin}"
      fi
    fi
  }
fi
ensure_java_home

# Fallback PATH helpers for shells that source this file before ~/.exports is updated.
if ! typeset -f path_prepend >/dev/null 2>&1; then
  _path_contains() {
    dir=$1
    if [[ -z ${dir:-} ]]; then
      return 1
    fi
    case ":${PATH:-}:" in
      *:"$dir":*) return 0 ;;
    esac
    return 1
  }

  path_prepend() {
    dir=$1
    if [[ -z ${dir:-} ]]; then
      return
    fi
    if ! _path_contains "$dir"; then
      if [[ -n ${PATH:-} ]]; then
        PATH="$dir:$PATH"
      else
        PATH="$dir"
      fi
      export PATH
    fi
  }

  path_append() {
    dir=$1
    if [[ -z ${dir:-} ]]; then
      return
    fi
    if ! _path_contains "$dir"; then
      if [[ -n ${PATH:-} ]]; then
        PATH="$PATH:$dir"
      else
        PATH="$dir"
      fi
      export PATH
    fi
  }
fi

# Initialize completion early so plugins calling compdef don't fail
if ! typeset -f compdef >/dev/null; then
  autoload -Uz compinit
  compinit
fi

# Preserve kitty remote-control socket inside nested shells/tmux
if [[ -n "${KITTY_WINDOW_ID:-}" ]]; then
  export KITTY_LISTEN_ON="${KITTY_LISTEN_ON:-unix:/tmp/kitty-$USER}"
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_DOTENV_FILE=.env.local
export ZSH_DISABLE_COMPFIX=true
POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context nodeenv virtualenv aws dir newline os_icon)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time vcs time)
POWERLEVEL9K_PROMPT_ON_NEWLINE=false
POWERLEVEL9K_RPROMPT_ON_NEWLINE=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
#POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="↳ "

export TERM="xterm-256color"
export DEFAULT_USER=gunnar

[[ -f $HOME/.ista_rc ]] && source "$HOME/.ista_rc"
[ -f ~/.private ] && source ~/.private

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"``

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew common-aliases direnv docker docker-compose dotenv extract fasd gem git git-lfs gitignore gpg-agent helm history history-substring-search kubectl macos mvn nvm pass sbt scala sdk screen virtualenv zsh-vi-mode)

source $ZSH/oh-my-zsh.sh

pman () {
    for name in "$@"
    do
        man -t $name | pstopdf -i
        mv -f stdin.pdf ~/Documents/man-pages/$name.pdf
        open ~/Documents/man-pages/$name.pdf
    done
}

mcd () {
  mkdir "$1" && cd "$1"
}

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# source ~/.aliases
export GTAGSLABEL=pygments

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Prefer personal and Homebrew-installed binaries.
[ -d /opt/homebrew/bin ] && path_prepend "/opt/homebrew/bin"
[ -d "$HOME/.bin" ] && path_prepend "$HOME/.bin"
[ -d "$HOME/go/bin" ] && path_prepend "$HOME/go/bin"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export NVM_DIR="$HOME/.nvm"
if [[ "$(uname)" == "Darwin" ]]; then
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
elif [[ "$(uname)" == "Linux" ]]; then
    [ -s "/usr/share/nvm/init-nvm.sh" ] && \. "/usr/share/nvm/init-nvm.sh"
fi

path_prepend "./node_modules/.bin"

if [ -d "$HOME/.emacs.doom/bin" ]; then
  path_append "$HOME/.emacs.doom/bin"
fi

[ -d /usr/local/opt/sqlite/bin ] && path_prepend "/usr/local/opt/sqlite/bin"

[[ -r "$HOME/.android-env" ]] && source "$HOME/.android-env"


if [ -d "$HOME/.rbenv/bin" ]; then
  path_prepend "$HOME/.rbenv/bin"
fi
eval "$(rbenv init - zsh)"

# eval "$(op completion zsh)"; compdef _op op

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/gunnar.bastkowski/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh"
    else
        if [ -d "/Users/gunnar.bastkowski/opt/anaconda3/bin" ]; then
            path_prepend "/Users/gunnar.bastkowski/opt/anaconda3/bin"
        fi
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# source /Users/gunnar.bastkowski/.config/op/plugins.sh

# Created by `pipx` on 2024-05-10 23:04:23
[ -d "$HOME/.local/bin" ] && path_append "$HOME/.local/bin"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
if [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]; then
  __sdkman_lazy_loaded=0
  __sdkman_lazy_init() {
    if (( __sdkman_lazy_loaded )); then
      return 0
    fi
    __sdkman_lazy_loaded=1
    source "$SDKMAN_DIR/bin/sdkman-init.sh"
  }

  sdk() {
    unset -f sdk
    if ! __sdkman_lazy_init; then
      echo "sdkman initialization failed" >&2
      return 1
    fi
    sdk "$@"
  }
fi

## [Completion]
## Completion scripts setup. Remove the following line to uninstall
[[ -f /home/gunnar/.dart-cli-completion/zsh-config.zsh ]] && . /home/gunnar/.dart-cli-completion/zsh-config.zsh || true
## [/Completion]

case "$(uname -a)" in
  *arch*)
      [[ -n "$SSH_CONNECTION" ]] && systemctl --user start inhibit-suspend.service
      ;;
  *Darwin*)
      # The following lines have been added by Docker Desktop to enable Docker CLI completions.
      fpath=(/Users/gunnar.bastkowski/.docker/completions $fpath)
      # compinit already called at the top of this file
      # End of Docker CLI completions
      ;;
esac

# Auto-start byobu for interactive shells (not already in tmux, and not in IntelliJ)
# if [[ $- == *i* ]] && [[ -z "$TMUX" ]] && [[ -z "$TERMINAL_EMULATOR" ]] && [[ -z "$INSIDE_EMACS" ]] && command -v byobu >/dev/null 2>&1; then
#     if [[ -n "$SSH_CONNECTION" ]]; then
#         # On remote hosts, attach to existing session or create new one
#         exec byobu new-session -A -s main
#     else
#         # On local host, just start byobu normally
#         exec byobu
#     fi
# fi

[ -f "$HOME/.local/bin/env" ] && . "$HOME/.local/bin/env"
