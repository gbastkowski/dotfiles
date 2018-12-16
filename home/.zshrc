export TERM="xterm-256color"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_MODE='nerdfont-complete'
# POWERLEVEL9K_MODE='awesome-patched'
#POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context root_indicator dir_writable dir)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time vcs time background_jobs)
POWERLEVEL9K_SHORTEN_STRATEGY=truncate_from_right
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''
POWERLEVEL9K_FOLDER_ICON=''

export DEFAULT_USER=gunnar.bastkowski

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
plugins=(
    aws
    brew brew-cask bundler
    colored-man common-aliases
    docker docker-compose docker-machine
    emacs extract
    fasd
    gem git git-flow github gitignore gpg-agent gradle
    history history-substring-search
    iterm
    jira
    mvn
    osx
    rake rake-fast rbenv ruby rvm
    sbt scala screen ssh-agent
    terminalapp terraform thefuck
    vagrant vault
)

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

source ~/.aliases
source ~/.exports

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

function chpwd() {
    if [ -r $PWD/.zsh_config ]; then
        source $PWD/.zsh_config
    fi
}

# added by travis gem
[ -f /Users/gunnar/.travis/travis.sh ] && source /Users/gunnar/.travis/travis.sh


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

eval "$(rbenv init -)"

source ~/.autosrc/autosrc.zsh
