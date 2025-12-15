# Load shared exports for any login shell.
if [ -r "$HOME/.exports" ]; then
  . "$HOME/.exports"
fi

# >>> coursier install directory >>>
path_append "/Users/gunnar.bastkowski/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<
