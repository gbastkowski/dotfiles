if [[ -r "$HOME/.profile" ]]; then
  source "$HOME/.profile"
fi

case "$OSTYPE" in
  darwin*)
      if [[ -x /opt/homebrew/bin/brew ]]; then
          brew_env_cache="${XDG_CACHE_HOME:-$HOME/.cache}/brew-shellenv.zsh"
          if mkdir -p "${brew_env_cache%/*}" 2>/dev/null; then
              if [[ ! -s $brew_env_cache || /opt/homebrew/bin/brew -nt $brew_env_cache ]]; then
                  if ! /opt/homebrew/bin/brew shellenv >| "$brew_env_cache" 2>/dev/null; then
                      eval "$(/opt/homebrew/bin/brew shellenv)"
                      brew_env_cache=""
                  fi
              fi
              if [[ -n $brew_env_cache && -r $brew_env_cache ]]; then
                  source "$brew_env_cache"
              fi
          else
              eval "$(/opt/homebrew/bin/brew shellenv)"
          fi
      fi

      source /Users/gunnar.bastkowski/.ista_profile
      ;;

  linux*)
      ;;

  *)
      echo "unknown: $OSTYPE"
      exit 1
      ;;
esac
