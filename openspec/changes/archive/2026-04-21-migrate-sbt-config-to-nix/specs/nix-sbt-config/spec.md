## ADDED Requirements

### Requirement: Manage global sbt configuration files via Home Manager
The system SHALL manage the three global sbt configuration files via `home.file` declarations in the shared Home Manager module, sourced from the repository-owned files under `sbt/.sbt/1.0/`.

#### Scenario: sbt config files are managed symlinks after activation
- **WHEN** a user activates the Home Manager configuration
- **THEN** `~/.sbt/1.0/build.sbt`, `~/.sbt/1.0/plugins/plugins.sbt`, and `~/.sbt/1.0/credentials.sbt` exist as Home Manager-managed symlinks pointing into the Nix store

### Requirement: Preserve runtime credential resolution in credentials.sbt
The system SHALL manage `credentials.sbt` as a static file whose content includes a runtime `pass` invocation, without evaluating or transforming that invocation at activation time.

#### Scenario: Artifactory credentials are resolved at sbt build time
- **WHEN** sbt loads the managed `~/.sbt/1.0/credentials.sbt`
- **THEN** the `pass` invocation executes and returns the Artifactory password at build time, not during Home Manager activation

### Requirement: Detect unmanaged sbt config collisions before activation
The system MUST stop activation when any of the three managed sbt files already exist as unmanaged files, consistent with Home Manager collision behaviour for other managed files.

#### Scenario: Existing unmanaged sbt files block activation
- **WHEN** a user activates the Home Manager configuration on a machine where any of the three sbt files exist as unmanaged files
- **THEN** activation stops before overwriting the existing files

### Requirement: Document collision handling for first sbt config activation
The system SHALL document the first-activation collision behaviour for the sbt config files, including that existing files must be backed up or moved aside before activation succeeds.

#### Scenario: Collision handling guidance is available for sbt config
- **WHEN** a user follows the setup documentation
- **THEN** the documentation explains how to handle existing sbt config files before the first activation
