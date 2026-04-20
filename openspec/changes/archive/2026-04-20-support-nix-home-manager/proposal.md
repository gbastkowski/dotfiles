## Why

The current dotfiles setup is repo-driven and accumulative, but it does not have a declarative, reproducible path for applying a small, clearly owned subset with Nix.
There is a historical `bootstrap.sh` path in the repository, but this change should not rely on it as the baseline workflow or compatibility target.
The long-term direction can still be broader Home Manager coverage, but the first slice should stay intentionally small so it can be introduced safely on both macOS and Arch Linux without depending on legacy setup paths.

## What Changes

- Add an optional standalone Home Manager-based setup path for this dotfiles repo on macOS and Arch Linux without depending on `bootstrap.sh`.
- Define the MVP as Home Manager ownership of exactly one zsh-related file: `zsh/.p10k.zsh`, because it is already separated from the main shell startup logic and is lower risk than the current `.zshrc` or `.zprofile`.
- Document that the MVP is additive rather than a full migration: Home Manager manages only the chosen file, and the rest of the repo remains outside Home Manager for now.
- Explicitly leave host-specific shell startup, private or machine-local files, nix-darwin integration, Linux desktop or service configuration, and broader package migration out of scope for this first slice.
- Position the MVP as the first step toward broader Home Manager coverage only after the ownership boundary and rollback path are proven.

## Capabilities

### New Capabilities
- `nix-home-manager-support`: Provide an optional Home Manager entry point for macOS and Arch Linux that can bootstrap Home Manager and manage a minimal, well-defined subset of this dotfiles repository without relying on legacy setup scripts.

### Modified Capabilities

## Impact

- New spec artifact at `openspec/changes/support-nix-home-manager/specs/nix-home-manager-support/spec.md`.
- New Home Manager bootstrap and configuration files for macOS and Arch Linux.
- Documentation and workflow boundaries around `README.org`, legacy setup paths, and the selected zsh file.
- First migration target is `zsh/.p10k.zsh`; the rest of the zsh startup stack remains outside Home Manager for the MVP.
- Future user-configuration areas may be brought under Home Manager incrementally after the MVP, including broader zsh configuration once host-specific and local dependencies are separated more cleanly.
