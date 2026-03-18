[ -r "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

[ -r "$HOME/.local/bin/ensure-local-bin-in-path" ] && . "$HOME/.local/bin/ensure-local-bin-in-path"

# Load shared exports for any login shell.
if [ -r "$HOME/.exports" ]; then
  . "$HOME/.exports"
fi

# >>> coursier install directory >>>
path_append "/Users/gunnar.bastkowski/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<

