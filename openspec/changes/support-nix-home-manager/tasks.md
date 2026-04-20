## 1. Platform prerequisites

- [x] 1.1 Document the MVP prerequisites for standalone Home Manager on macOS and Arch Linux, including Nix installation and flake support.
- [x] 1.2 Decide and encode the exact supported `system` values for the current macOS architecture and the Arch Linux target.

## 2. Repository Home Manager structure

- [x] 2.1 Add the repository-owned flake entry point under `nix/flake.nix`.
- [x] 2.2 Add the shared Home Manager module under `nix/home/common.nix` with the minimal base Home Manager configuration.
- [x] 2.3 Add thin target-specific modules for macOS and Arch Linux that provide only the required platform, username, and home-directory values.

## 3. MVP file management

- [x] 3.1 Configure Home Manager to manage exactly one target file, `~/.p10k.zsh`, via `home.file` sourced from the repository-owned `zsh/.p10k.zsh`.
- [x] 3.2 Verify that `.zshrc`, `.zprofile`, `programs.zsh`, and broader shell startup behavior remain outside the MVP Home Manager scope.

## 4. Activation and collision handling

- [x] 4.1 Document first-activation collision handling for an existing unmanaged `~/.p10k.zsh`, including backup or move-aside guidance.
- [x] 4.2 Document the supported first activation flow, including build-before-switch and any required staging step for flake-tracked files.
- [x] 4.3 Document rollback expectations for restoring a previous `~/.p10k.zsh` if the MVP setup is removed.

## 5. Validation

- [ ] 5.1 Validate the flake and Home Manager configuration without activation.
- [ ] 5.2 Validate first activation on the supported macOS architecture and confirm that the existing shell setup continues sourcing `~/.p10k.zsh`.
- [ ] 5.3 Validate first activation on Arch Linux and confirm that `~/.p10k.zsh` is the only file brought under Home Manager management in the MVP.
