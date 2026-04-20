## ADDED Requirements

### Requirement: Manage git identity and core settings via Home Manager
The system SHALL configure git user identity, core settings, pull/merge/push behaviour, and LFS support via `programs.git` in the shared Home Manager module.

#### Scenario: Git identity is set by Home Manager
- **WHEN** a user activates the Home Manager configuration
- **THEN** `~/.gitconfig` contains the correct user name, email, and signing key as managed entries

#### Scenario: Git LFS filter is active
- **WHEN** a user activates the Home Manager configuration
- **THEN** `git lfs install` behaviour is available and the LFS filter is registered in the managed git config

### Requirement: Manage global git ignore patterns via Home Manager
The system SHALL express all global git ignore patterns via `programs.git.ignores`, replacing the manually deployed `git/.gitignore_global`.

#### Scenario: Global ignores are applied without a separate ignore file
- **WHEN** a user activates the Home Manager configuration
- **THEN** the managed `~/.gitconfig` sets `core.excludesfile` to the Home Manager-generated ignores file and the patterns from `git/.gitignore_global` are present

### Requirement: Detect unmanaged gitconfig collision before activation
The system MUST stop activation when an unmanaged `~/.gitconfig` already exists, consistent with Home Manager collision behaviour for other managed files.

#### Scenario: Existing unmanaged `.gitconfig` blocks activation
- **WHEN** a user activates the Home Manager configuration on a machine where `~/.gitconfig` exists as an unmanaged file
- **THEN** activation stops before overwriting the existing file

### Requirement: Document collision handling for first git config activation
The system SHALL document the first-activation collision behaviour for `~/.gitconfig`, including that the existing file must be backed up or moved aside before activation succeeds.

#### Scenario: Collision handling guidance is available for git config
- **WHEN** a user follows the setup documentation
- **THEN** the documentation explains how to handle an existing `~/.gitconfig` before the first activation
