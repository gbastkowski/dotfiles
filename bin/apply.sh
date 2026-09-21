#!/usr/bin/env bash
set -euo pipefail

DOTFILES="$(cd "$(dirname "$0")/.." && pwd)"

PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"

host="${DOTFILES_HOSTNAME:-$(hostname -s)}"

case "$host" in
  deess1mac*) target="ista-dotfiles" ;;
  akiko*)     target="akiko-dotfiles" ;;
  *) echo "unknown host: $host; set DOTFILES_HOSTNAME"; exit 1 ;;
esac

exec home-manager switch -b backup --flake "${DOTFILES}#${target}" "$@"
