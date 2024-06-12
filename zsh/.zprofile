case "$OSTYPE" in
  darwin*)
      eval "$(/opt/homebrew/bin/brew shellenv)"

      # >>> coursier install directory >>>
      export PATH="$PATH:/Users/gunnar.bastkowski/Library/Application Support/Coursier/bin"
      # <<< coursier install directory <<<
      source /opt/homebrew/opt/autoenv/activate.sh
      ;;

  linux*)
      ;;

  *)
      echo "unknown: $OSTYPE"
      exit 1
      ;;
esac
