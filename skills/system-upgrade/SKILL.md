---
name: system-upgrade
description: Run the full dotfiles system upgrade via bin/system-upgrade.sh — OS package updates, dotfiles git pull, home-manager switch, and doom emacs upgrade. Use when the user asks to upgrade the system, update the dotfiles, or run the system upgrade.
license: MIT
metadata:
  author: Gunnar Bastkowski
  version: "0.1"
---

# system-upgrade

Run the full machine upgrade: bin/system-upgrade.sh in the dotfiles repo.

## How to run

Prefer the system_upgrade MCP tool (mcp-dotfiles server). If the server is
not available, run the script directly:

bash ~/git/gbastkowski/dotfiles/bin/system-upgrade.sh

## What it does (in order)

1. Host-specific OS package update:
   - macOS (deess1mac*): softwareupdate -l, brew update && brew upgrade, pipx upgrade-all
   - Arch Linux (akiko*): yay -Syu, hyprpm update (or pkg update && pkg upgrade on Android), pipx upgrade-all
2. npm global updates: ccline, tweakcc, openspec
3. git pull --rebase origin main in the dotfiles repo; if the script itself
   was updated it re-execs itself once
4. bin/apply.sh — home-manager switch (see the apply-dotfiles skill)
5. doom upgrade + doom sync -u (when doom is on PATH)

## Caveats

- **Long-running**: 10+ minutes is normal. Do not interrupt — a killed
  home-manager switch can leave a half-applied generation.
- The git pull --rebase fails on uncommitted local changes; commit or stash
  first if the pull fails.
- Unknown hosts (hostname not matching deess1mac* / akiko*) abort loudly.

## After the run

- Report the final git status (the script prints it itself).
- If the home-manager switch failed: home-manager rollback restores the
  previous generation; check home-manager generations for the last good one.
