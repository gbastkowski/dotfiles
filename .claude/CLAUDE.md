# Claude Config

## Repository Context
Comprehensive dotfiles for macOS/Linux development using git submodules.

## Directories
- `claude/` - Global Claude configuration for all projects
- `.claude/` - Project-specific Claude configuration for this dotfiles repository
- `bin/` - Custom scripts and utilities
- `emacs/` - Emacs configuration files
- `git/` - Git configuration
- `home/` - Home directory dotfiles
- `hypr/` - Hyprland configuration
- `plover/` - Plover stenography configuration
- `powerlevel10k/` - PowerLevel10k theme (submodule)
- `reveal.js/` - Reveal.js presentation framework (submodule)
- `sbt/` - SBT build tool configuration
- `scala/` - Scala development configuration
- `tmux/` - Tmux configuration
- `zsh/` - ZSH shell configuration
- `zsh-vi-mode/` - ZSH Vi mode plugin (submodule)

## Active Components
- ZSH/Oh-My-ZSH shell configuration
- Doom Emacs (config.org is the only relevant Emacs configuration)
- Hyprland/Yabai window management
- tmux terminal multiplexer
- Homebrew package management
- Custom bin/ scripts
- Cross-platform compatibility (macOS/Linux)
- Automated deployment

## Deprecated Components
**Note:** Stow and Spacemacs configurations are deprecated and can be ignored.

## Environment Context
Detect if running inside Emacs window by checking:
1. Emacs env vars (`INSIDE_EMACS`, `EMACS`)
2. Terminal in Emacs shell/term mode

**Inside Emacs:**
- Use `emacsclient` for file operations
- Leverage projectile/project.el for project management
- Respect buffer/window management

**Outside Emacs:**
- Standard terminal operations
- Shell-based project detection

## Git Guidelines
- Do not add the co-authored line to git commit messages
- Never include Claude attribution, "Generated with Claude Code" messages, or AI credits in git commits, Jira issues, GitLab merge requests, pull requests, or any project documentation

## GitHub Integration
Use `gh` CLI for issues when requested.

## File Changes
Auto-revert enabled - no manual buffer reverting needed.
