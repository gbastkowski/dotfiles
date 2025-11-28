# Repository Guidelines

This is a dotfiles repository for macOS and Linux. It manages shell, editor, and system configuration via ~bootstrap.sh~ (rsync into ~$HOME~) and a few helper scripts. Keep changes minimal, reversible, and host‑agnostic.

## Project Structure & Module Organization
- `bin/` — utility scripts (system upgrades, helpers).
- `bash/`, `zsh/`, `tmux/`, `emacs/`, `hypr/`, etc. — config packages copied or symlinked into `$HOME` via `bootstrap.sh`.
- `reveal.js/`, `powerlevel10k/`, `zsh-vi-mode/`, `emacs/*` — submodules; update via `bin/system-upgrade.sh`.
- `bootstrap.sh`, `brew.sh` — setup/installation scripts (legacy Stow installer was removed).

## Build, Test, and Development Commands
- `./bootstrap.sh [-n|--dry-run]` — rsync selected files to `$HOME` (interactive by default).
- `bin/system-upgrade.sh` — update OS packages and submodules, then commit dotfiles.
- macOS packages: edit `brew.sh` and run manually if needed.

## Coding Style & Naming Conventions
- Shell: POSIX sh/bash where practical; prefer `#!/usr/bin/env bash` for bash features.
- Indentation: 2 spaces; avoid tabs in new code.
- Use safe flags (`set -euo pipefail`) for new scripts when compatible.
- Paths: never hardcode user‑specific absolute paths; use `$HOME`, `$(dirname "$0")`.
- Filenames: lowercase with dashes, e.g., `checkmail.sh`.

## Testing Guidelines
- No formal test suite. Validate changes by:
  - Running scripts with dry‑run flags where available (e.g., `bootstrap.sh -n`).
  - Optional: run `shellcheck bin/*.sh` locally to catch issues.

## Commit & Pull Request Guidelines
- Commits: concise, imperative subject (≤72 chars). Examples:
  - "Update dotfiles and submodules"
  - "Fix ZSH initialization errors"
- PRs: describe the scope, affected packages (e.g., `zsh`, `emacs`), platform impact (macOS/Linux), and testing steps/screenshots/logs when relevant. Link related issues if any.

## Security & Configuration Tips
- Do not commit secrets. Use `~/.private` (sourced in zsh) and `password-store/` submodule for secrets management.
- Submodules may require SSH access. Ensure keys are configured before running `bin/system-upgrade.sh`.

## Agent-Specific Instructions
- Changes must be idempotent, host‑agnostic, and avoid destructive ops.
- Ensure new files are copied by `bootstrap.sh` (adjust excludes only if absolutely required).
