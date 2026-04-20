## Why

The global sbt configuration (`~/.sbt/1.0/build.sbt`, `plugins.sbt`, `credentials.sbt`) is currently deployed manually or via symlinks outside any declarative system.
Migrating it to Home Manager via `home.file` brings it under the same reproducible path established by the Nix MVP.

## What Changes

- Add `home.file` declarations to `nix/home/common.nix` for the three global sbt config files:
  - `~/.sbt/1.0/build.sbt` sourced from `sbt/.sbt/1.0/build.sbt`
  - `~/.sbt/1.0/plugins/plugins.sbt` sourced from `sbt/.sbt/1.0/plugins/plugins.sbt`
  - `~/.sbt/1.0/credentials.sbt` sourced from `sbt/.sbt/1.0/credentials.sbt`
- The existing files remain in `sbt/` for reference until migration is validated, then can be removed

## Capabilities

### New Capabilities

- `nix-sbt-config`: Home Manager management of global sbt settings, plugins, and Artifactory credentials via `home.file`

### Modified Capabilities

- `nix-home-manager-support`: The shared Home Manager module gains `home.file` entries for sbt; the managed surface expands further

## Impact

- `nix/home/common.nix`: gains three `home.file` entries
- `sbt/.sbt/1.0/build.sbt`, `plugins.sbt`, `credentials.sbt`: superseded by Home Manager symlinks; can be removed after validation
- No impact on zsh, git, or other dotfile areas
- `credentials.sbt` contains a runtime `pass` invocation — Home Manager symlinks the file as-is; the secret is resolved by sbt at build time, not at activation time
