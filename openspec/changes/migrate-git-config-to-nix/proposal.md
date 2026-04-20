## Why

The git configuration (`git/.gitconfig` and `git/.gitignore_global`) is currently deployed manually or via symlinks outside of any declarative system.
Migrating it to Home Manager via `programs.git` brings it under the same reproducible, version-controlled management path established by the Nix MVP.

## What Changes

- Replace `git/.gitconfig` with a `programs.git` block in `nix/home/common.nix`
- Replace `git/.gitignore_global` with `programs.git.ignores` in the same module
- The existing `git/.gitconfig` and `git/.gitignore_global` files remain in the repository for reference until the migration is validated, then can be removed
- `core.excludesfile` no longer needs to be set manually; Home Manager sets it automatically when `programs.git.ignores` is used

## Capabilities

### New Capabilities

- `nix-git-config`: Home Manager management of git user identity, core settings, pull/merge/push behaviour, LFS filter, and global ignores via `programs.git`

### Modified Capabilities

- `nix-home-manager-support`: The shared Home Manager module gains a `programs.git` block; the managed surface expands beyond `.p10k.zsh`

## Impact

- `nix/home/common.nix`: gains `programs.git` configuration
- `git/.gitconfig`: superseded by Home Manager output; can be removed after validation
- `git/.gitignore_global`: superseded by `programs.git.ignores`; can be removed after validation
- No impact on zsh, tmux, or other dotfile areas
- Git LFS filter must be expressed as a `programs.git.extraConfig` block since Home Manager does not have a first-class LFS option
