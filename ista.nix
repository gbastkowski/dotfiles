{ inputs, ... }:
{
  home.username = "gunnar.bastkowski";
  home.homeDirectory = "/Users/gunnar.bastkowski";

  home.file.".aliases".source = ./home/.aliases;

  home.file.".m2/settings.xml".source = ./mvn/settings.xml;

  home.file.".ista_profile".source = "${inputs.ista-dotfiles}/.ista_profile";
  home.file.".ista_rc".source = "${inputs.ista-dotfiles}/.ista_rc";

  home.sessionVariables = {
    LIBRARY_PATH = "/opt/homebrew/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin25/15";
  };

  programs.zsh.initContent = ''
    # Homebrew
    if [[ -x /opt/homebrew/bin/brew ]]; then
      brew_env_cache="''${XDG_CACHE_HOME:-$HOME/.cache}/brew-shellenv.zsh"
      mkdir -p "''${brew_env_cache%/*}" 2>/dev/null
      if [[ ! -s $brew_env_cache || /opt/homebrew/bin/brew -nt $brew_env_cache ]]; then
        /opt/homebrew/bin/brew shellenv >| "$brew_env_cache" 2>/dev/null || eval "$(/opt/homebrew/bin/brew shellenv)"
      fi
      [[ -r $brew_env_cache ]] && source "$brew_env_cache"
      path_prepend "/opt/homebrew/bin"
    fi

    # sqlite
    [[ -d /usr/local/opt/sqlite/bin ]] && path_prepend "/usr/local/opt/sqlite/bin"

    # iterm2
    test -e "''${HOME}/.iterm2_shell_integration.zsh" && source "''${HOME}/.iterm2_shell_integration.zsh"

    # Docker Desktop completions
    if [[ -d /Users/gunnar.bastkowski/.docker/completions ]]; then
      fpath=(/Users/gunnar.bastkowski/.docker/completions $fpath)
    fi

    # ista profile
    [[ -f $HOME/.ista_profile ]] && source "$HOME/.ista_profile"

    # conda
    __conda_setup="$('/Users/gunnar.bastkowski/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2>/dev/null)"
    if [ $? -eq 0 ]; then
      eval "$__conda_setup"
    elif [ -f "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh" ]; then
      source "/Users/gunnar.bastkowski/opt/anaconda3/etc/profile.d/conda.sh"
    elif [ -d "/Users/gunnar.bastkowski/opt/anaconda3/bin" ]; then
      path_prepend "/Users/gunnar.bastkowski/opt/anaconda3/bin"
    fi
    unset __conda_setup

    # Coursier
    [[ -d "$HOME/Library/Application Support/Coursier/bin" ]] && path_append "$HOME/Library/Application Support/Coursier/bin"
  '';
}
