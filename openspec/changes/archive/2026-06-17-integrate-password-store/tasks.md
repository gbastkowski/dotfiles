## 1. Remote setup

- [x] 1.1 Create bare repo `ssh-w00d23c2@bastkowski.name:git/password-store` (sibling of org), default branch `main`
- [x] 1.2 On primary machine: repointed `origin` from keybase to bastkowski, renamed `master`→`main`, pushed

## 2. Follow-up GPG issue

- [x] 2.1 Opened GitHub issue gbastkowski/dotfiles#35 "Add GPG key init to dotfiles setup"; URL: https://github.com/gbastkowski/dotfiles/issues/35

## 3. Nix module

- [x] 3.1 Created `password-store.nix` (sibling of `byobu.nix`) exporting a home-manager module
- [x] 3.2 Added `home.packages = [ pkgs.pass pkgs.gnupg ];` in the new module
- [x] 3.3 Defined `home.activation.bootstrapPasswordStore = lib.hm.dag.entryAfter [ "writeBoundary" ] ''…'';` with hardcoded `REMOTE` URL constant
- [x] 3.4 Reference `gpg` + `git` via Nix store paths in activation (no PATH-presence check needed)
- [x] 3.5 Inside activation: check `gpg --list-secret-keys --with-colons | grep -q '^sec:'`; exit non-zero referencing the follow-up issue URL if no key
- [x] 3.6 Inside activation: clone if `~/.password-store` absent; remote-add if present + no origin; warn + non-zero if mismatched origin; silent no-op otherwise
- [x] 3.7 Import `password-store.nix` from `flake.nix` (added to `commonModules`)

## 4. Documentation

- [x] 4.1 Added "Password store" section to `README.org`: prerequisites, behavior summary, manual sync workflow, link to gbastkowski/dotfiles#35

## 5. Verification

- [x] 5.1 Ran `home-manager switch --flake .#ista-dotfiles` with configured store; activation completed, bootstrap silent (no-op branch)
- [ ] 5.2 Deferred: run `home-manager switch` on a fresh machine missing `~/.password-store`; confirm clone succeeds and `pass ls` works (needs second host)
- [ ] 5.3 Deferred: simulate missing GPG secret key (temporary keyring) and confirm activation aborts non-zero with issue URL (needs disposable env)
- [x] 5.4 Idempotency confirmed: switch ran twice; second run silent + zero exit
- [x] 5.5 `/opsx:verify` re-run pending after these task updates
