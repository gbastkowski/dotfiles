case "$OSTYPE" in
  darwin*)
      eval "$(/opt/homebrew/bin/brew shellenv)"

      # >>> coursier install directory >>>
      export PATH="$PATH:/Users/gunnar.bastkowski/Library/Application Support/Coursier/bin"
      # <<< coursier install directory <<<

      autoload -U +X compinit && compinit
      autoload -U +X bashcompinit && bashcompinit

      source /Users/gunnar.bastkowski/.ista_profile
      ;;

  linux*)
      ;;

  *)
      echo "unknown: $OSTYPE"
      exit 1
      ;;
esac
