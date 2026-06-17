# password-store-sync Specification

## Purpose
TBD - created by archiving change integrate-password-store. Update Purpose after archive.
## Requirements
### Requirement: Auto-bootstrap via home-manager activation
The system SHALL bootstrap `~/.password-store` automatically as part of the home-manager activation phase, on both macOS and Linux.
No manual ad-hoc script invocation SHALL be required.

#### Scenario: Fresh machine with no existing store
- **WHEN** `home-manager switch` runs and `~/.password-store` does not exist
- **THEN** the activation script clones the configured remote into `~/.password-store`

#### Scenario: Existing local store without remote
- **WHEN** `home-manager switch` runs and `~/.password-store` exists as a git repo with no `origin` remote
- **THEN** the activation script adds the configured remote as `origin` and leaves working tree untouched

#### Scenario: Existing store with mismatched remote
- **WHEN** `home-manager switch` runs and `~/.password-store` already has an `origin` remote that differs from the configured remote
- **THEN** the activation script prints a warning to stderr and exits non-zero, aborting the switch

### Requirement: GPG secret key prerequisite aborts activation when unmet
The activation script SHALL verify that at least one GPG secret key is available before any filesystem changes,
and MUST abort the activation with a non-zero exit and an actionable error message (including a link to the follow-up issue covering GPG init) when the check fails.
The `gpg` binary itself is provided by the Nix store path the activation script references directly, so a separate "binary missing" check is unnecessary.

#### Scenario: No GPG secret key available
- **WHEN** `gpg --list-secret-keys --with-colons` produces no `sec:` line
- **THEN** the activation script exits non-zero with a message instructing the user to import or generate a GPG secret key, and references the follow-up GitHub issue

### Requirement: Activation is idempotent
The activation script SHALL be safe to run on every home-manager switch without altering an already-correctly-configured store.

#### Scenario: Re-running on configured machine
- **WHEN** `~/.password-store` exists, is a git repo, and `origin` matches the configured remote
- **AND** `home-manager switch` runs a second time
- **THEN** the activation script makes no changes and produces no warnings

### Requirement: pass and gnupg available after activation
The home-manager configuration SHALL ensure that `pass` and `gnupg` are installed (via `home.packages` or equivalent) so that the user can interact with the bootstrapped store immediately after activation.

#### Scenario: Post-activation tooling
- **WHEN** a user opens a shell after a successful `home-manager switch`
- **THEN** `pass` and `gpg` are on `PATH`

### Requirement: Setup documented in repository
The repository README SHALL include a "Password store" section describing the auto-bootstrap behavior, prerequisites (GPG secret key), and the manual `pass git push` / `pass git pull` sync workflow.

#### Scenario: New user reads README
- **WHEN** a user reads the dotfiles README looking for password-store setup
- **THEN** they find a section explaining that activation handles bootstrap, what the GPG prerequisite is, and how to push/pull changes manually

