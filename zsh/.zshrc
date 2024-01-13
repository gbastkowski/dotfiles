# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_DOTENV_FILE=.env.local
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

if [ -f ~/.private ]
then
    source ~/.private
fi

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

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
plugins=(1password autoenv aws brew common-aliases docker docker-compose dotenv emacs extract fasd gem git git-lfs gitignore gpg-agent helm history history-substring-search kubectl macos mvn sbt scala sdk screen zsh-vi-mode)

source $ZSH/oh-my-zsh.sh

pman () {
    for name in "$@"
    do
        man -t $name | pstopdf -i
        mv -f stdin.pdf ~/Documents/man-pages/$name.pdf
        open ~/Documents/man-pages/$name.pdf
    done
}

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# source ~/.aliases
# source ~/.exports
export GTAGSLABEL=pygments

eval "$(fasd --init auto)"

eval $(thefuck --alias)

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH=/opt/homebrew/bin/python3/opt/homebrew/bin/python3:~/.bin:~/go/bin:$PATH

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# export JAVA_HOME=$(/usr/libexec/java_home -v 11)

alias aga='aws-google-auth -k --bg-response js_enabled -p'
alias aga-all='aga default && aga dev-admin && aga stg-admin && aga prd'

if [ -f ~/.bashrc ]
then
    . ~/.bashrc
fi

export ANDROID_HOME=/Users/gunnar.bastkowski/Library/Android/sdk/
export PATH="$ANDROID_HOME/tools/bin:$PATH"
export PATH="$ANDROID_HOME/platform-tools:$PATH"

export PATH="$HOME/.rbenv/bin:$PATH"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - zsh)"

eval "$(op completion zsh)"; compdef _op op

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/gunnar.bastkowski/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/gunnar.bastkowski/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

source /Users/gunnar.bastkowski/.config/op/plugins.sh
