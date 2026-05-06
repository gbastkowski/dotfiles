{ inputs, ... }:
{
  home.username = "gunnar.bastkowski";
  home.homeDirectory = "/Users/gunnar.bastkowski";

  home.file.".aliases".source = ../../home/.aliases;

  home.file.".ista_profile".source = "${inputs.ista-dotfiles}/.ista_profile";
  home.file.".ista_rc".source = "${inputs.ista-dotfiles}/.ista_rc";

  home.file.".claude/CLAUDE.md".source = ../../claude/CLAUDE.md;
  home.file.".claude/settings.json".source = ../../claude/settings.json;
  home.file.".claude/statusline.sh".source = ../../claude/statusline.sh;
  home.file.".claude/commands".source = ../../claude/commands;
  home.file.".claude/agents".source = ../../claude/agents;
  home.file.".config/opencode/opencode.json".source = ../../opencode/opencode.json;
  home.file.".config/opencode/oh-my-openagent.json".source = ../../opencode/oh-my-openagent.json;

  home.file."Library/Application Support/plover/plover.cfg".source = ../../plover/plover.cfg;
  home.file."Library/Application Support/plover/main.json".source = ../../plover/main.json;
  home.file."Library/Application Support/plover/user.json".source = ../../plover/user.json;
  home.file."Library/Application Support/plover/commands.json".source = ../../plover/commands.json;
  home.file."Library/Application Support/plover/lapwing-base.json".source = ../../plover/lapwing-base.json;
  home.file."Library/Application Support/plover/lapwing-commands.json".source = ../../plover/lapwing-commands.json;
  home.file."Library/Application Support/plover/lapwing-movement.modal".source = ../../plover/lapwing-movement.modal;
  home.file."Library/Application Support/plover/lapwing-numbers.json".source = ../../plover/lapwing-numbers.json;
  home.file."Library/Application Support/plover/lapwing-proper-nouns.json".source = ../../plover/lapwing-proper-nouns.json;
  home.file."Library/Application Support/plover/lapwing-uk-additions.json".source = ../../plover/lapwing-uk-additions.json;
  home.file."Library/Application Support/plover/abby-left-hand-modifiers.py".source = ../../plover/abby-left-hand-modifiers.py;
  home.file."Library/Application Support/plover/emily-modifiers.py".source = ../../plover/emily-modifiers.py;
  home.file."Library/Application Support/plover/emily-symbols.py".source = ../../plover/emily-symbols.py;
  home.file."Library/Application Support/plover/jeff-phrasing.py".source = ../../plover/jeff-phrasing.py;
  home.file.".config/kitty/kitty.conf".source = ../../kitty/kitty.conf;
  home.file.".config/kitty/current-theme.conf".source = ../../kitty/current-theme.conf;

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

    # nvm via homebrew
    [[ -s "/opt/homebrew/opt/nvm/nvm.sh" ]] && source "/opt/homebrew/opt/nvm/nvm.sh"
    [[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ]] && source "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"

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
