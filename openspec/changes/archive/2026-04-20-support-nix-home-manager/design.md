## Context

This change introduces a new, standalone Home Manager path for this dotfiles repository.
The repository currently has no native Nix or Home Manager structure, so the MVP needs to add one without assuming any existing declarative setup.

The chosen MVP target is `zsh/.p10k.zsh`.
That choice is grounded in the current repo layout: `zsh/.zshrc` sources `~/.p10k.zsh` as a separate unit, while `zsh/.zshrc` and `zsh/.zprofile` also contain host-specific logic, local/private sourcing, platform branches, and absolute paths.
Managing `.p10k.zsh` first gives the change a narrow ownership boundary and avoids pulling broader shell startup behavior into scope.

The MVP must work on macOS and Arch Linux.
Home Manager should therefore be used in standalone mode rather than via nix-darwin integration, because the Linux target requires standalone support and the proposal explicitly excludes nix-darwin from the first slice.

## Goals / Non-Goals

**Goals:**
- Provide a standalone Home Manager entry point that can be used on macOS and Arch Linux.
- Keep the first managed surface to exactly one file: `zsh/.p10k.zsh`.
- Keep platform-specific differences limited to the minimum required user and system metadata.
- Keep the rest of the repository outside Home Manager for the MVP.
- Make rollout and rollback explicit, especially where an existing `~/.p10k.zsh` would collide with the managed target.

**Non-Goals:**
- Migrating `zsh/.zshrc`, `zsh/.zprofile`, or the broader zsh plugin/theme stack.
- Replacing historical setup scripts or treating them as the compatibility baseline.
- Introducing nix-darwin integration, system package management, or Linux desktop/service management.
- Solving secrets, machine-local paths, or host-specific shell startup in this change.
- Completing a full dotfiles migration to Home Manager.

## Decisions

### 1. Use standalone Home Manager as the MVP architecture

The MVP will use standalone Home Manager on both supported platforms.
This keeps the architecture aligned across macOS and Arch Linux and avoids making the macOS path depend on nix-darwin while Linux uses a different model.

**Why this over nix-darwin integration first?**
- nix-darwin is macOS-specific and would not help with the Arch Linux part of the MVP.
- The proposal explicitly treats nix-darwin integration as out of scope for the first slice.
- A single standalone approach keeps the documentation and rollout story simpler.

### 2. Use a repository-owned flake-based Home Manager layout

The MVP should add a small flake-based Home Manager layout to the repository.
The flake provides a clear repo-local entry point, explicit inputs, and an easier path to future cross-platform growth than a channel-based ad hoc setup.

The expected structure is:
- `nix/flake.nix` as the Home Manager entry point
- `nix/home/common.nix` for shared MVP configuration
- one thin per-target module for platform- or target-specific values

The per-target modules should be limited to values such as:
- Nix `system`
- `home.username`
- `home.homeDirectory`

All actual MVP behavior should stay in the shared module.

**Why this over channel-based standalone Home Manager?**
- Channel-based setup is simpler in the short term, but it weakens reproducibility and pushes more setup state outside the repository.
- The repository currently has no Nix structure, so adding one repo-owned entry point is more valuable than preserving a non-reproducible bootstrap path.

**Trade-off:**
- Flakes are still marked experimental in Nix tooling.
- For this repository, that trade-off is acceptable because the MVP is explicitly additive and repo-centered.

### 3. Manage the first file via `home.file`, not via `programs.zsh`

The MVP should manage `~/.p10k.zsh` using a direct Home Manager file declaration.
That keeps the managed surface to one file and matches the current repository boundary.

The intended configuration shape is:

```nix
home.file.".p10k.zsh".source = ../zsh/.p10k.zsh;
```

The exact relative path may vary depending on the final file layout, but the design principle is fixed: the Home Manager config should link the existing repo-owned prompt config directly.

**Why this over `programs.zsh.enable = true;`?**
- `programs.zsh` manages a larger shell surface than the MVP allows.
- The current goal is not “manage zsh”, but “manage one chosen dotfile safely”.
- `home.file` gives a cleaner, narrower ownership boundary for the first rollout.

### 4. Treat collisions as an explicit migration step

Home Manager will refuse to overwrite an existing target file that it does not own.
Because `~/.p10k.zsh` likely already exists on machines using this repo, first activation must include a documented migration step.

The MVP design should therefore assume:
- existing `~/.p10k.zsh` may need to be moved aside or backed up before the first `home-manager switch`
- rollback must restore the previous file or disable the Home Manager-owned link cleanly

This is not an implementation detail to ignore later; it is part of the MVP rollout contract.

### 5. Keep platform variance shallow and explicit

The design should avoid platform-specific logic in the shared module.
Platform differences should be limited to target selection and user/home-directory metadata.

For the MVP, the expected differences are:
- Linux vs Darwin `system` values in the flake output
- `/home/<user>` vs `/Users/<user>` for `home.homeDirectory`
- prerequisite documentation for enabling flakes and installing Home Manager in standalone mode

For macOS, the MVP only needs to support the architecture currently in use.
Intel macOS support is explicitly out of scope for this change.

Everything else should remain shared.

## Risks / Trade-offs

- [Existing `~/.p10k.zsh` collides with Home Manager target] → Require an explicit first-activation migration step and document rollback before rollout.
- [Flake-based setup adds conceptual overhead] → Keep the flake layout minimal, with one shared module and thin target wrappers only.
- [The repo still has mixed legacy setup paths] → Keep the MVP strictly additive and avoid making any claim that non-Home-Manager paths are canonical or validated.
- [Scope expands from prompt config into full shell management] → Keep `programs.zsh`, `.zshrc`, and `.zprofile` out of scope in both specs and tasks.
- [Platform differences leak into shared config] → Constrain platform-specific values to target modules and keep the shared module file-oriented.

## Migration Plan

1. Add the repository-owned Home Manager structure for the MVP.
2. Add one shared module that manages `zsh/.p10k.zsh` through `home.file`.
3. Add thin target definitions for macOS and Arch Linux with the required `system`, username, and home-directory values.
4. Document first-time setup prerequisites and the collision-handling step for an existing `~/.p10k.zsh`.
5. Validate that `~/.p10k.zsh` is Home Manager-managed and still sourced correctly by the existing `.zshrc`.
6. Roll back by disabling the Home Manager-owned file and restoring the previous `~/.p10k.zsh` from backup if needed.

## Open Questions

- No open questions at this stage.
