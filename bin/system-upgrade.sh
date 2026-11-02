#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" && pwd)"
# The Home Manager-installed copy lives in ~/.local/bin, not in the repo.
if [ -z "${DOTFILES_DIR:-}" ]; then
  DOTFILES_DIR="$(cd "$SCRIPTPATH/.." && pwd)"
  if [ ! -f "$DOTFILES_DIR/flake.nix" ]; then
    DOTFILES_DIR="$HOME/git/gbastkowski/dotfiles"
  fi
fi
ORIGINAL_DIR="$(pwd)"

review=0
for arg in "$@"; do
  case "$arg" in
    --review) review=1 ;;
    -h|--help)
      echo "Usage: $0 [--review]"
      echo "  --review  Preview failed-step stderr and unread Home Manager news for an optional LLM review."
      exit 0 ;;
    *) echo "Unknown option: $arg" >&2; exit 2 ;;
  esac
done

review_dir="${SYSTEM_UPGRADE_REVIEW_DIR:-}"
if [ "$review" -eq 1 ]; then
  if [ -z "$review_dir" ]; then
    review_dir="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-upgrade-review.XXXXXX")" || exit 1
    : > "$review_dir/errors"
  fi
  # Only these two known files are created here, including across re-exec.
  trap 'rm -f "$review_dir/stderr" "$review_dir/errors"; rmdir "$review_dir"' EXIT
fi

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
  local status
  if [ "$review" -eq 1 ]; then
    # FD 3 keeps stdout out of the capture; only stderr goes through tee.
    { "$@" 2>&1 1>&3 | tee "$review_dir/stderr" >&2; status=${PIPESTATUS[0]}; } 3>&1
  else
    "$@"
    status=$?
  fi
  if [ "$status" -ne 0 ]; then
    echo "FAILED: $* (exit $status)" >&2
    failed+=("$*")
    if [ "$review" -eq 1 ]; then
      {
        printf '\nStep: %s (exit %s)\n' "$*" "$status"
        # Redact before clipping in the helper, so partial secrets cannot leak.
        cat "$review_dir/stderr"
        printf '\n'
      } >> "$review_dir/errors"
    fi
  fi
  [ "$review" -eq 0 ] || rm -f "$review_dir/stderr"
  return 0
}

case "$host" in
  deess1mac*)
    hm_target="ista-dotfiles"
    step softwareupdate -l
    step brew update
    step brew upgrade
    step pipx upgrade-all
    ;;
  akiko*)
    hm_target="akiko-dotfiles"
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
		SYSTEM_UPGRADE_REVIEW_DIR="$review_dir" SYSTEM_UPGRADE_REEXEC=1 SYSTEM_UPGRADE_FAILED="$(printf '%s\n' "${failed[@]}")" exec "$DOTFILES_DIR/bin/system-upgrade.sh" "$@"
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

if [ "$review" -eq 1 ]; then
  if command -v python3 >/dev/null 2>&1; then
    python3 "$SCRIPTPATH/system-upgrade-review.py" "$DOTFILES_DIR" "$hm_target" "$review_dir/errors" \
      || echo "Upgrade review failed; upgrade results are unchanged." >&2
  else
    echo "Upgrade review skipped: python3 is not installed." >&2
  fi
fi

cd "$ORIGINAL_DIR" || exit 1
echo
if [ "${#failed[@]}" -gt 0 ]; then
	echo "finished with ${#failed[@]} failed step(s):" >&2
	printf '  - %s\n' "${failed[@]}" >&2
	exit 1
fi
echo "done :-)"
