## Context

The repository manages global sbt configuration under `sbt/.sbt/1.0/`:
- `build.sbt` — global settings (semanticdb, onChangedBuildSource)
- `plugins/plugins.sbt` — global plugins (sbt-updates, dependency-tree, semanticdb compiler plugin)
- `credentials.sbt` — Artifactory credentials resolved at sbt runtime via `pass`

These are currently deployed outside any declarative system.
This change adds `home.file` entries to `nix/home/common.nix`, consistent with how `~/.p10k.zsh` was migrated in the MVP.

## Goals / Non-Goals

**Goals:**
- Manage all three sbt config files via `home.file` in the shared Home Manager module
- Keep the existing `sbt/` directory in the repo as the source of truth for file content
- Remove the files from `sbt/` after validation

**Non-Goals:**
- Managing sbt installation via Home Manager (`programs.sbt` or similar)
- Managing per-project sbt config
- Changing the content of any sbt config file

## Decisions

### 1. Use `home.file` for all three files

The sbt files are static dotfiles with no platform-specific values.
`home.file` is the correct primitive — the same approach used for `.p10k.zsh`.
No first-class `programs.sbt` option exists in Home Manager that covers global user config.

### 2. Keep `credentials.sbt` in scope

The file contains a runtime `pass` shell invocation to retrieve the Artifactory password.
Home Manager symlinks the file as text — the `pass` call is evaluated by sbt at build time, not during activation.
There is no secrets management concern at the Home Manager layer.

### 3. No platform splits needed

All three files are identical across macOS and Arch Linux.
The declarations belong in `nix/home/common.nix` only.

## Risks / Trade-offs

- [Existing `~/.sbt/1.0/build.sbt` etc. collide with managed targets] → Move aside before first switch, same pattern as previous migrations.
- [sbt creates additional files under `~/.sbt/1.0/` at runtime] → Home Manager only manages the three declared files; other runtime-generated files are unaffected.

## Migration Plan

1. Add `home.file` entries to `nix/home/common.nix`.
2. Move aside existing `~/.sbt/1.0/build.sbt`, `~/.sbt/1.0/plugins/plugins.sbt`, and `~/.sbt/1.0/credentials.sbt`.
3. Run `home-manager build` to validate, then `home-manager switch`.
4. Confirm the three files are managed symlinks.
5. Remove `sbt/.sbt/1.0/build.sbt`, `sbt/.sbt/1.0/plugins/plugins.sbt`, and `sbt/.sbt/1.0/credentials.sbt` from the repository.

## Open Questions

- No open questions at this stage.
