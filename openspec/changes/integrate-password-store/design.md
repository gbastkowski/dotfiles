## Context

`pass` (zx2c4) stores secrets as GPG-encrypted files under `~/.password-store/`.
Default install = local only; `pass git ...` wraps a git repo inside that dir.
Dotfiles are managed by home-manager (Nix flake, see `flake.nix`, `common.nix`, per-app `*.nix` modules).
Activation scripts already used (see `byobu.nix` `home.activation.patchByobuTmuxrc`) for idempotent post-install patching.

GPG keys currently NOT managed by dotfiles — out of scope here. Tracked as a follow-up GitHub issue.

Current state:
- macOS: `pass` + `gnupg` available via Nix or Homebrew.
- Linux: same via Nix / native pkg manager.
- No remote configured.
- No bootstrap mechanism.

Stakeholders: single user (Gunnar) across personal macOS + Linux machines.

## Goals / Non-Goals

**Goals:**
- Auto-bootstrap `~/.password-store` on every home-manager activation.
- Zero manual ad-hoc invocation — runs as part of the normal dotfiles update flow.
- Idempotent: no churn when already configured.
- Fail loud + actionable when GPG key missing (don't silently skip).
- Works on macOS + Linux from the same Nix expression.

**Non-Goals:**
- GPG key generation/distribution (tracked as separate follow-up issue).
- Auto-sync (`pass git push/pull` stays manual).
- Multi-user / team store.
- Browser bridges, Emacs/shell integrations beyond what `pass` ships.
- Standalone `bin/` bootstrap script — explicitly rejected by user.

## Decisions

### D1: Bootstrap via home-manager activation script
- **Choice**: Add `password-store.nix` with `home.activation.bootstrapPasswordStore = lib.hm.dag.entryAfter [ "writeBoundary" ] ''…''`, following `byobu.nix` pattern.
- **Why**: Runs on every `home-manager switch` / dotfiles update with no user action. Matches existing repo idiom. No new install surface.
- **Alternatives**:
  - `bin/` script (original draft) — rejected: user does not run ad-hoc scripts on other machines.
  - ZSH init hook — runs per-shell, wastes cycles; harder to abort loudly.
  - `system-upgrade.sh` — coarser, only runs when explicitly invoked; activation runs on every switch.

### D2: Remote host = private GitHub repo
- **Choice**: `git@github.com:gbastkowski/password-store.git` (or env override).
- **Why**: Already on GitHub, `gh` CLI authed, cheap, reliable.
- **Risk**: Entry-name metadata visible to GitHub. Accepted; switch to `passage` (age) later if needed.

### D3: Remote URL hardcoded in nix expression
- **Choice**: Single hardcoded URL constant in `password-store.nix`. No env override.
- **Why**: Personal dotfiles, one remote, no test scenarios that require swapping. Keeps activation logic simple.

### D4: Activation logic
Reference `gpg` and `git` via their Nix store paths (`${pkgs.gnupg}/bin/gpg`, `${pkgs.git}/bin/git`) — no PATH dependency, no "binary missing" check needed.

Sequence inside the activation script:
1. Check `gpg --list-secret-keys --with-colons | grep -q '^sec:'` — abort with non-zero exit + reference to follow-up issue URL if no secret key.
2. If `~/.password-store` absent: `git clone "$REMOTE" "$HOME/.password-store"`.
3. Else if not a git repo: warn, exit non-zero.
4. Else if no `origin` remote: `git -C "$HOME/.password-store" remote add origin "$REMOTE"`.
5. Else if `origin` URL ≠ `$REMOTE`: print warning to stderr, exit non-zero.
6. Else: silent no-op (already configured).
- `pass` itself is NOT required at activation time (script uses plain `git`), so a transient missing `pass` package will not block bootstrap.

### D5: Abort on missing GPG = hard fail
- **Choice**: Non-zero exit from activation block, which fails the home-manager switch.
- **Why**: User explicitly chose "abort, and create a gh issue to add gpg init to my setup". Hard fail forces fix instead of silent drift.
- **Mitigation**: Follow-up GitHub issue tracks adding GPG key init to dotfiles, so the gap is visible.

### D6: Package additions
- Add `pass` + `gnupg` to `home.packages` in `common.nix` (or new module) so they are guaranteed present after activation, even though activation itself only needs `git` + `gpg`.

## Risks / Trade-offs

- **Risk**: Hard-fail on missing GPG could brick home-manager switch on a fresh machine before keys are imported → Mitigation: documented in README. Accept the friction; the follow-up issue (GPG init) is the real fix.
- **Risk**: Network failure during clone fails activation → Mitigation: re-run `home-manager switch` once network restored; activation is idempotent.
- **Risk**: Force-push or remote rewrite corrupts history → Mitigation: activation never force-pushes; user does `pass git` manually.
- **Trade-off**: Manual `pass git pull/push` means secrets diverge across machines until user pulls. Accepted; auto-sync = scope creep.
- **Trade-off**: Activation runs even when user doesn't care about pass on a given host → Acceptable; cost is one `git remote -v` call.

## Migration Plan

1. Create private GitHub repo `gbastkowski/password-store`.
2. On primary machine: ensure existing `~/.password-store` is a git repo; add remote; push to `main`.
3. Open follow-up GitHub issue "Add GPG key init to dotfiles setup" (referenced from activation error message + README).
4. Land `password-store.nix` + README update.
5. Run `home-manager switch` on each other machine; verify `pass ls` works after.

Rollback: remove `password-store.nix` import from `flake.nix`; next switch leaves `~/.password-store` untouched (activation is additive).

## Open Questions

- Branch name: `main`.
- Should activation also configure `pass git` user.name/email locally? → No; inherits global git config.
