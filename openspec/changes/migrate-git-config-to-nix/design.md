## Context

The repository currently manages git configuration via two files deployed outside any declarative system:
- `git/.gitconfig` — user identity, core settings, pull/merge/push behaviour, LFS filter
- `git/.gitignore_global` — global ignore patterns (Emacs, macOS, JVM tooling, IDE artefacts)

The Nix Home Manager MVP (`nix/home/common.nix`) currently manages only `~/.p10k.zsh`.
This change extends the shared module with `programs.git`, which generates `~/.gitconfig` and sets `core.excludesfile` automatically.

## Goals / Non-Goals

**Goals:**
- Express all current `git/.gitconfig` settings declaratively via `programs.git` in `nix/home/common.nix`
- Express all current `git/.gitignore_global` patterns via `programs.git.ignores`
- Keep the change confined to `nix/home/common.nix`; no per-platform modules need changing

**Non-Goals:**
- Per-machine or per-platform git identity overrides
- Managing work vs personal git identity switching
- Removing `git/.gitconfig` and `git/.gitignore_global` from the repository before validation is complete

## Decisions

### 1. Use `programs.git` in the shared module

All current git settings are platform-agnostic (same user, same email, same behaviour on macOS and Arch Linux).
The `programs.git` block therefore belongs entirely in `nix/home/common.nix`.

**Why not split across platform modules?**
There is no platform-specific git behaviour in the current config.
Splitting would add complexity without benefit.

### 2. Express LFS filter via `programs.git.lfs.enable`

Home Manager provides `programs.git.lfs.enable = true` which generates the correct `[filter "lfs"]` block automatically.
This is cleaner than duplicating the four LFS filter lines in `extraConfig`.

**Why not `extraConfig`?**
`programs.git.lfs.enable` is the idiomatic Home Manager option and generates the exact same output.
Using `extraConfig` for something that has a first-class option would be inconsistent.

### 3. Keep `signingkey` in `programs.git.signing` but leave `gpg.program` unset

The current config sets a signing key but does not configure commit auto-signing.
The MVP should replicate the current behaviour exactly: key is registered, auto-sign is not forced.

### 4. Retain `git/.gitconfig` and `git/.gitignore_global` until after first activation

Home Manager will generate `~/.gitconfig` as a managed symlink.
If the unmanaged `~/.gitconfig` already exists, activation will stop (same collision behaviour as `.p10k.zsh`).
The repo files are kept for reference and as a rollback reference until activation is validated.

**Migration step required:** move aside or back up the existing `~/.gitconfig` before the first `home-manager switch`.

## Risks / Trade-offs

- [Existing `~/.gitconfig` collides with managed target] → Document move-aside step; use `-b backup` flag during first switch.
- [Email in config is work-specific] → This is already the case in the repo file; no change in exposure.
- [`programs.git.lfs` may not be available in older Home Manager releases] → The flake pins `release-25.11` which includes this option.

## Migration Plan

1. Add `programs.git` block to `nix/home/common.nix`.
2. Move aside existing `~/.gitconfig` on the target machine.
3. Run `home-manager build` to validate, then `home-manager switch`.
4. Confirm git identity and LFS filter are active (`git config --list`).
5. After validation, remove `git/.gitconfig` and `git/.gitignore_global` from the repository.

## Open Questions

- No open questions at this stage.
