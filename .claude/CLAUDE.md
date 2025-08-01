# Claude Config

## Repository Context
Comprehensive dotfiles for macOS/Linux development.
Uses git submodules.
Main components: ZSH/Oh-My-ZSH, Doom Emacs (only relevant Emacs config), Hyprland/Yabai window management, tmux, Homebrew.
Features automated deployment, custom bin/ scripts, cross-platform compatibility.

**Note:** Stow and Spacemacs configurations are deprecated and can be ignored.

## Session Init
Keep responses brief, avoid repeating information.

## Writing Style
For markdown, org-mode and similar formats: sentences should start on new lines.

## Environment Detection
Detect if running inside Emacs window. Check:
1. Emacs env vars (`INSIDE_EMACS`, `EMACS`)
2. Terminal in Emacs shell/term mode

## Context
**Inside Emacs:**
- Use `emacsclient` for file ops
- Leverage projectile/project.el
- Doom Emacs config: config.org (only relevant Emacs configuration)
- Respect buffer/window management

**Outside Emacs:**
- Standard terminal operations
- Shell-based project detection

## GitHub
Use `gh` CLI for issues when requested.

## File Changes
Auto-revert enabled - no manual buffer reverting needed.

## Development Environment
- stow and spacemacs are deprecated and can be ignored

## Git Guidelines
- Do not add the co-authored line to git commit messages
