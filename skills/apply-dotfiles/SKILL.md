---
name: apply-dotfiles
description: Apply the home-manager dotfiles configuration via bin/apply.sh — home-manager switch --flake with the host-specific target. Use when dotfiles (nix modules or deployed config) changed and the user wants the new configuration live.
license: MIT
metadata:
  author: Gunnar Bastkowski
  version: "0.1"
---

# apply-dotfiles

Make the current dotfiles configuration live with bin/apply.sh.

## How to run

Prefer the apply MCP tool (mcp-dotfiles server). If the server is not
available, run the script directly:

bash ~/git/gbastkowski/dotfiles/bin/apply.sh

## What it does

- Detects the host: deess1mac* → ista-dotfiles, akiko* → akiko-dotfiles
- Runs home-manager switch -b backup --flake <dotfiles>#<target>

The -b backup flag keeps a backup generation, so the switch is reversible.

## When to use

- After editing any *.nix module or a config file deployed via home.file
  (e.g. opencode/, claude/, emacs/, tmux/, hypr/).
- The user says apply the dotfiles, switch, or rebuild my config.

## Caveats

- **Long-running**: a full switch (with activation) can take minutes.
- Files deployed via home-manager are **read-only Nix store symlinks**; never
  edit the deployed copy in ~/.config/... — edit the source in the repo and
  apply.
- The switch can be silent for a while between phases; do not interrupt.

## After the run

- Verify: home-manager generations shows the new generation; git status in
  the dotfiles repo is clean (unless the switch created *.backup files, which
  are gitignored).
- On failure: home-manager rollback returns to the previous generation.
