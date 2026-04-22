## Why

The powerlevel10k theme is currently managed as a git submodule (`powerlevel10k/`) symlinked to `~/.oh-my-zsh/custom/themes/powerlevel10k`.
Git submodules are brittle: they require explicit initialisation on every clone, are easy to forget to update, and add friction to the repository setup.
Replacing the submodule with a Nix flake input gives reproducible pinning, single-command updates via `nix flake update`, and removes the submodule entirely.

## What Changes

- Add `powerlevel10k` as a flake input (`flake = false`) in `nix/flake.nix` pointing to `github:romkatv/powerlevel10k`
- Add a `home.file` entry in `nix/home/common.nix` to place the theme at `~/.oh-my-zsh/custom/themes/powerlevel10k`
- Remove the `powerlevel10k` git submodule from the repository
- Remove the existing manual symlink `~/.oh-my-zsh/custom/themes/powerlevel10k → dotfiles/powerlevel10k`

## Capabilities

### New Capabilities

- `nix-powerlevel10k`: Nix flake input and Home Manager file declaration for the powerlevel10k theme

### Modified Capabilities

- `nix-home-manager-support`: The flake gains a new external input; the shared module gains a `home.file` entry for the theme

## Impact

- `nix/flake.nix`: gains `powerlevel10k` input
- `nix/home/common.nix`: gains `home.file.".oh-my-zsh/custom/themes/powerlevel10k"` sourced from the flake input
- `.gitmodules`: `powerlevel10k` submodule entry removed
- `powerlevel10k/`: submodule directory removed from repository
- `~/.oh-my-zsh/custom/themes/powerlevel10k`: changes from a manual symlink to a Home Manager-managed symlink into the Nix store
- No changes to `.zshrc` — `ZSH_THEME="powerlevel10k/powerlevel10k"` continues to work unchanged
