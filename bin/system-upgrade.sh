#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(cd "$SCRIPTPATH/.." && pwd)"
ORIGINAL_DIR="$(pwd)"

cd "$DOTFILES_DIR" || { echo "Error: Cannot find dotfiles directory at $DOTFILES_DIR"; exit 1; }

case "$(hostname -s)" in
  deess1mac*)
    softwareupdate -l
    brew update && brew upgrade
    pipx upgrade-all
    HM_TARGET="ista-dotfiles"
    ;;
  akiko*)
    case "$(uname -a)" in
      *Android*) pkg update && pkg upgrade ;;
      *)         yay -Syu && hyprpm update ;;
    esac
    pipx upgrade-all
    HM_TARGET="akiko-dotfiles"
    ;;
  *)
    echo "unknown host: $(hostname -s)"; exit 1 ;;
esac

if command -v npm >/dev/null 2>&1; then
	echo "updating ccline (npm) ..."
	npm update -g @cometix/ccline      || echo "warning: failed to update ccline"
	npm update -g tweakcc              || echo "warning: failed to update tweakcc"
	npm update -g @fission-ai/openspec || echo "warning: failed to update openspec"
	echo
fi

echo "pulling dotfiles ..."
git pull --rebase origin main
echo

echo "switching home-manager configuration ..."
home-manager switch --flake "$DOTFILES_DIR#$HM_TARGET"
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

cd "$ORIGINAL_DIR" || exit 1
echo
echo "done :-)"
