## Context

The powerlevel10k theme is currently a git submodule at `powerlevel10k/` in the repository root, manually symlinked to `~/.oh-my-zsh/custom/themes/powerlevel10k`.
Oh-my-zsh loads it via `ZSH_THEME="powerlevel10k/powerlevel10k"` in `.zshrc`.

The Nix flake at `nix/flake.nix` already has `nixpkgs` and `home-manager` as inputs.
Adding `powerlevel10k` as a third input (with `flake = false`) is the minimal change needed.

## Goals / Non-Goals

**Goals:**
- Replace the submodule with a flake input pinned to `github:romkatv/powerlevel10k`
- Place the theme at `~/.oh-my-zsh/custom/themes/powerlevel10k` via `home.file`
- Remove the submodule from `.gitmodules` and the repository
- Keep `.zshrc` unchanged

**Non-Goals:**
- Migrating oh-my-zsh itself to Nix
- Migrating zsh-vi-mode or other submodules in this change
- Pinning to a specific tag — track the default branch for now, update via `nix flake update`

## Decisions

### 1. Use a flake input with `flake = false`

powerlevel10k is not a Nix flake, so `flake = false` is required to prevent Nix from trying to evaluate it as one.
The input resolves to a store path that can be used directly as a `home.file` source.

```nix
inputs.powerlevel10k = {
  url = "github:romkatv/powerlevel10k";
  flake = false;
};
```

### 2. Pass inputs through to the Home Manager module

The flake's `outputs` function currently takes `{ nixpkgs, home-manager, ... }`.
To make `inputs.powerlevel10k` available in `common.nix`, pass it as an `extraSpecialArgs`:

```nix
homeManagerConfiguration {
  extraSpecialArgs = { inherit inputs; };
  modules = [ ./home/common.nix ./home/darwin.nix ];
};
```

Then `common.nix` receives `inputs` as an argument:

```nix
{ inputs, ... }: {
  home.file.".oh-my-zsh/custom/themes/powerlevel10k".source = inputs.powerlevel10k;
}
```

### 3. Remove the submodule cleanly

Submodule removal requires three steps:
1. `git submodule deinit -f powerlevel10k`
2. `git rm -f powerlevel10k`
3. Remove `.git/modules/powerlevel10k`

The manual symlink `~/.oh-my-zsh/custom/themes/powerlevel10k` must also be removed before the first `home-manager switch` to avoid collision.

## Risks / Trade-offs

- [Collision with existing manual symlink] → Remove `~/.oh-my-zsh/custom/themes/powerlevel10k` before first switch.
- [Tracking default branch means implicit updates] → `nix flake update` is explicit and controlled; no silent updates happen.
- [First `nix flake update` after migration fetches latest commit] → Acceptable; pin to a tag later if stability is needed.

## Migration Plan

1. Add `powerlevel10k` flake input to `nix/flake.nix`.
2. Thread `inputs` via `extraSpecialArgs` in both `homeManagerConfiguration` blocks.
3. Add `home.file` entry in `common.nix`.
4. Remove the submodule from the repository.
5. Remove the existing manual symlink at `~/.oh-my-zsh/custom/themes/powerlevel10k`.
6. Run `home-manager switch` and confirm the theme loads.

## Open Questions

- No open questions at this stage.
