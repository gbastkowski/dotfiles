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
    ;;
  akiko*)
    case "$(uname -a)" in
      *Android*) pkg update && pkg upgrade ;;
      *)         yay -Syu && hyprpm update ;;
    esac
    pipx upgrade-all
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

if [ -z "$SYSTEM_UPGRADE_REEXEC" ]; then
	echo "pulling dotfiles ..."
	before="$(git rev-parse HEAD)"
	git pull --rebase origin main
	after="$(git rev-parse HEAD)"
	echo

	if [ "$before" != "$after" ]; then
		echo "system-upgrade.sh updated, restarting ..."
		echo
		SYSTEM_UPGRADE_REEXEC=1 exec "$DOTFILES_DIR/bin/system-upgrade.sh" "$@"
	fi
fi

echo "switching home-manager configuration ..."
"$DOTFILES_DIR/bin/apply.sh"
echo

echo "updating doom emacs ..."
if command -v doom >/dev/null 2>&1; then
	doom upgrade
	doom sync -u
else
	echo "doom not found, skipping"
fi

echo "current state:"
git status

cd "$ORIGINAL_DIR" || exit 1
echo
echo "done :-)"
