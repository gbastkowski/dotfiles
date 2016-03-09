ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_MODE=''
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status vcs time)

export DEFAULT_USER=gunnar

alias zshconfig="emacsclient ~/.zshrc"
alias ohmyzsh="emacsclient ~/.oh-my-zsh"

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
plugins=(brew brew-cask colored-man common-aliases emacs extract gem git gitignore gradle history history-substring-search mvn osx sbt scala screen ssh-agent teminalapp vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=~/.bin:/usr/local/bin:$PATH:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/MacGPG2/bin:/Library/TeX/texbin

pman () { man -t "$@" | open -f -a Preview; }

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

alias epost='source ~/Projects/E-Post/Config/epost.sh'

source ~/.aliases
source ~/.exports
