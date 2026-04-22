# Spec: Nix Powerlevel10k

## Purpose

Defines the requirements for providing the powerlevel10k ZSH theme via a Nix flake input, replacing the git submodule approach.

## Requirements

### Requirement: Provide powerlevel10k theme via Nix flake input
The system SHALL declare powerlevel10k as a flake input with `flake = false` in `nix/flake.nix`, pointing to `github:romkatv/powerlevel10k`.

#### Scenario: powerlevel10k flake input exists
- **WHEN** a user inspects `nix/flake.nix`
- **THEN** a `powerlevel10k` input with `flake = false` pointing to `github:romkatv/powerlevel10k` is present

### Requirement: Place powerlevel10k theme at the oh-my-zsh custom themes path via Home Manager
The system SHALL manage `~/.oh-my-zsh/custom/themes/powerlevel10k` as a Home Manager-managed symlink sourced from the `inputs.powerlevel10k` flake input.

#### Scenario: Theme is available at the expected oh-my-zsh path after activation
- **WHEN** a user activates the Home Manager configuration
- **THEN** `~/.oh-my-zsh/custom/themes/powerlevel10k` exists as a Home Manager-managed symlink into the Nix store

### Requirement: Preserve existing zsh theme loading without changes to .zshrc
The system SHALL ensure the existing `ZSH_THEME="powerlevel10k/powerlevel10k"` setting in `.zshrc` continues to load the theme without modification.

#### Scenario: zsh loads powerlevel10k theme after migration
- **WHEN** a new zsh session starts after Home Manager activation
- **THEN** the powerlevel10k theme loads correctly via the existing `ZSH_THEME` setting

### Requirement: Remove the powerlevel10k git submodule
The system SHALL remove the `powerlevel10k` git submodule from the repository after the Nix flake input is in place and validated.

#### Scenario: powerlevel10k submodule is absent after migration
- **WHEN** a user inspects the repository after migration
- **THEN** the `powerlevel10k` submodule entry is absent from `.gitmodules` and the `powerlevel10k/` directory is not present
