## Why

`pass` store at `~/.password-store` lives only on one machine.
No remote, no sync.
New laptop or Linux box = re-init from scratch, secrets diverge.
User does not run ad-hoc bootstrap scripts on other machines regularly, so setup must piggyback on the existing dotfiles update flow (home-manager activation).

## What Changes

- Configure `~/.password-store` as git repo with remote (private GitHub).
- Add a home-manager activation script (`password-store.nix`) that bootstraps `~/.password-store` automatically on every home-manager switch / dotfiles update.
- Activation is idempotent: no-op when already configured.
- Abort activation if `gpg` or a GPG secret key is missing, and open (or surface) a GitHub issue tracking "add gpg init to setup" so the gap gets filled in a follow-up change.
- Document setup in repo README.
- macOS + Linux parity: same Nix expression, same remote.

## Capabilities

### New Capabilities
- `password-store-sync`: Auto-bootstrap of git-backed `~/.password-store` via home-manager activation, with GPG-key prerequisite check that aborts loudly when unmet.

### Modified Capabilities
<!-- none -->

## Impact

- New file: `password-store.nix` imported from `flake.nix` (or per-host nix file).
- README section on password-store sync.
- Depends on `pass` + `gnupg` available at activation time → add to `home.packages` if not already present.
- New follow-up GitHub issue: "Add GPG key init to dotfiles setup" (blocks first-time activation on fresh boxes until resolved).
- No new `bin/` script.
