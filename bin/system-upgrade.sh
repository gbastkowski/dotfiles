#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(cd "$SCRIPTPATH/.." && pwd)"
ORIGINAL_DIR="$(pwd)"

cd "$DOTFILES_DIR" || { echo "Error: Cannot find dotfiles directory at $DOTFILES_DIR"; exit 1; }

host="${DOTFILES_HOSTNAME:-$(hostname -s)}"

# Run a step; on failure remember it and carry on, so one broken step does
# not skip the rest. The summary at the end reports everything that failed.
failed=()
if [ -n "$SYSTEM_UPGRADE_FAILED" ]; then
  # failures from before the re-exec below
  while IFS= read -r line; do failed+=("$line"); done <<< "$SYSTEM_UPGRADE_FAILED"
fi

step() {
  "$@" || { echo "FAILED: $*" >&2; failed+=("$*"); }
}

case "$host" in
  deess1mac*)
    step softwareupdate -l
    step brew update
    step brew upgrade
    step pipx upgrade-all
    ;;
  akiko*)
    case "$(uname -a)" in
      *Android*) step pkg update; step pkg upgrade ;;
      *)
        step yay -Syu
        # herdr panes do not inherit the compositor's env, so derive it at call time and skip when headless.
        HYPRLAND_INSTANCE_SIGNATURE="$(hyprctl instances -j 2>/dev/null | jq -r '.[0].instance // empty')"
        export HYPRLAND_INSTANCE_SIGNATURE
        if [ -n "$HYPRLAND_INSTANCE_SIGNATURE" ]
        then step hyprpm update
        else echo "skipping hyprpm update (no running Hyprland instance)"
        fi
        ;;
    esac
    step pipx upgrade-all
    ;;
  *)
    echo "unknown host: $host; set DOTFILES_HOSTNAME"; exit 1 ;;
esac

if command -v npm >/dev/null 2>&1; then
	echo "updating ccline (npm) ..."
	step npm update -g @cometix/ccline
	step npm update -g tweakcc
	step npm update -g @fission-ai/openspec
	echo
fi

if command -v opencode >/dev/null 2>&1; then
	echo "clearing opencode cache ..."
	rm -rf "$HOME/.cache/opencode"
	echo
fi

if [ -z "$SYSTEM_UPGRADE_REEXEC" ]; then
	echo "pulling dotfiles ..."
	before="$(git rev-parse HEAD)"
	step git pull --rebase origin main
	after="$(git rev-parse HEAD)"
	echo

	if [ "$before" != "$after" ]; then
		echo "system-upgrade.sh updated, restarting ..."
		echo
		SYSTEM_UPGRADE_REEXEC=1 SYSTEM_UPGRADE_FAILED="$(printf '%s\n' "${failed[@]}")" exec "$DOTFILES_DIR/bin/system-upgrade.sh" "$@"
	fi
fi

echo "switching home-manager configuration ..."
step "$DOTFILES_DIR/bin/apply.sh"
echo

echo "updating doom emacs ..."
if command -v doom >/dev/null 2>&1; then
	step doom upgrade
	step doom sync -u
else
	echo "doom not found, skipping"
fi

echo "current state:"
git status

cd "$ORIGINAL_DIR" || exit 1
echo
if [ "${#failed[@]}" -gt 0 ]; then
	echo "finished with ${#failed[@]} failed step(s):" >&2
	printf '  - %s\n' "${failed[@]}" >&2
	exit 1
fi
echo "done :-)"
