#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(cd "$SCRIPTPATH/.." && pwd)"
ORIGINAL_DIR="$(pwd)"

# Change to dotfiles directory
cd "$DOTFILES_DIR" || {
	echo "Error: Cannot find dotfiles directory at $DOTFILES_DIR"
	exit 1
}

OSTYPE="$("$SCRIPTPATH/ostype.sh")"

case "$OSTYPE" in
    arch*)   . "$SCRIPTPATH/linux.include.sh" ;;
    darwin*) . "$SCRIPTPATH/macos.include.sh" ;;
    termux*) . "$SCRIPTPATH/termux.include.sh" ;;
    *)       echo "unknown: $OSTYPE"; exit 1 ;;
esac

upgrade_system_and_packages

upgrade_python_packages

if command -v npm >/dev/null 2>&1; then
	echo "updating ccline (npm) ..."
	npm update -g @cometix/ccline      || echo "warning: failed to update ccline"
	npm update -g tweakcc              || echo "warning: failed to update tweakcc"
	npm update -g @fission-ai/openspec || echo "warning: failed to update openspec"
	echo
else
	echo "npm not found, skipping ccline update"
	echo
fi

echo "pulling dotfiles ..."
git pull --rebase origin main
echo

echo "switching home-manager configuration ..."
case "$OSTYPE" in
    arch*)    home-manager switch --flake "$DOTFILES_DIR/nix#arch-dotfiles" ;;
    darwin*)  home-manager switch --flake "$DOTFILES_DIR/nix#darwin-dotfiles" ;;
esac
echo

echo "updating doom emacs ..."
if command -v doom >/dev/null 2>&1; then
	doom upgrade
	doom sync
else
	echo "doom not found, skipping"
fi

echo "current state:"
git status

# Return to original directory
cd "$ORIGINAL_DIR" || exit 1

echo
echo "done :-)"
