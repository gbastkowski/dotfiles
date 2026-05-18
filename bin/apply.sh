#!/usr/bin/env bash
set -euo pipefail

DOTFILES="$(cd "$(dirname "$0")/.." && pwd)"

case "$(hostname -s)" in
  deess1mac*) target="ista-dotfiles" ;;
  akiko*)     target="akiko-dotfiles" ;;
  *) echo "unknown host: $(hostname -s)"; exit 1 ;;
esac

exec home-manager switch -b backup --flake "${DOTFILES}#${target}" "$@"
