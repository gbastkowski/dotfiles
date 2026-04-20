# Spec: Nix Home Manager Support

## Purpose

Defines the requirements for integrating Nix Home Manager into the dotfiles repository, starting with a minimal MVP that manages a single file target on macOS and Arch Linux.

## Requirements

### Requirement: Provide standalone Home Manager entry points for the MVP
The system SHALL provide a standalone Home Manager setup path for the MVP on macOS and Arch Linux without requiring nix-darwin integration.

#### Scenario: Standalone entry points exist for supported platforms
- **WHEN** a user inspects the repository for the MVP Home Manager setup
- **THEN** the repository contains standalone Home Manager entry points for macOS and Arch Linux

### Requirement: Limit the MVP managed surface to `.p10k.zsh`
The system SHALL limit the MVP Home Manager-managed surface to exactly one file target at `~/.p10k.zsh`, sourced from the repository-owned `zsh/.p10k.zsh`.

#### Scenario: MVP manages exactly one zsh-related file
- **WHEN** the user inspects the MVP Home Manager configuration
- **THEN** it manages `~/.p10k.zsh` and does not manage additional zsh startup files as part of the MVP

### Requirement: Keep broader zsh startup behavior outside the MVP
The system MUST NOT require Home Manager to manage `.zshrc`, `.zprofile`, `.zshenv`, or other broader shell startup behavior for this MVP.

#### Scenario: Existing shell startup remains outside Home Manager scope
- **WHEN** the MVP Home Manager setup is reviewed
- **THEN** `.zshrc` and `.zprofile` remain outside Home Manager management and continue to define broader shell startup behavior

### Requirement: Preserve existing prompt sourcing behavior
The system SHALL preserve the current behavior where the interactive zsh configuration sources `~/.p10k.zsh` as a separate prompt configuration file.

#### Scenario: Existing zsh prompt sourcing remains valid
- **WHEN** `~/.p10k.zsh` is managed by the MVP Home Manager setup
- **THEN** the existing zsh configuration can continue sourcing `~/.p10k.zsh` without requiring broader zsh migration

### Requirement: Detect unmanaged file collisions before activation changes
The system MUST detect whether an unmanaged `~/.p10k.zsh` already exists before applying the Home Manager-managed target and MUST stop activation before changing managed files when such a collision is present.

#### Scenario: Existing unmanaged `.p10k.zsh` blocks activation
- **WHEN** a user activates the MVP Home Manager setup on a machine where `~/.p10k.zsh` already exists as an unmanaged file
- **THEN** activation stops before applying the managed target

### Requirement: Document collision handling for first activation
The system SHALL document the first-activation collision behavior for `~/.p10k.zsh`, including that an existing file may need to be backed up or moved aside before activation succeeds.

#### Scenario: Collision handling guidance is available
- **WHEN** a user follows the MVP setup documentation
- **THEN** the documentation explains how to handle an existing `~/.p10k.zsh` before the first activation

### Requirement: Restrict macOS MVP support to the current architecture
The system SHALL support Arch Linux and the currently used macOS architecture for the MVP and MUST NOT require Intel macOS support in this change.

#### Scenario: Intel macOS is out of scope
- **WHEN** the MVP platform support is reviewed
- **THEN** Intel macOS support is not required by this change
