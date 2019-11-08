ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel10k/powerlevel10k"
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
plugins=(archlinux aws common-aliases docker docker-compose dotenv emacs extract fasd gem git gitignore gpg-agent gradle helm history history-substring-search kubectl mvn sbt scala screen vagrant)

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
