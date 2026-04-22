## 1. Update the Nix flake

- [ ] 1.1 Add `powerlevel10k` input with `flake = false` pointing to `github:romkatv/powerlevel10k` in `nix/flake.nix`.
- [ ] 1.2 Thread `inputs` via `extraSpecialArgs` in both `homeManagerConfiguration` blocks in `nix/flake.nix`.

## 2. Update the Home Manager module

- [ ] 2.1 Add `{ inputs, ... }:` argument to `nix/home/common.nix` and add `home.file.".oh-my-zsh/custom/themes/powerlevel10k".source = inputs.powerlevel10k;`.

## 3. Remove the submodule

- [ ] 3.1 Run `git submodule deinit -f powerlevel10k` to deregister the submodule.
- [ ] 3.2 Run `git rm -f powerlevel10k` to remove the submodule from the index and working tree.
- [ ] 3.3 Remove `.git/modules/powerlevel10k` to clean up the submodule metadata.

## 4. Collision handling and first activation

- [ ] 4.1 Remove the existing manual symlink `~/.oh-my-zsh/custom/themes/powerlevel10k`.
- [ ] 4.2 Run `home-manager build --flake ./nix#gunnar-dotfiles-darwin` to validate.
- [ ] 4.3 Run `home-manager switch --flake ./nix#gunnar-dotfiles-darwin` and confirm `~/.oh-my-zsh/custom/themes/powerlevel10k` is a managed symlink into the Nix store.
- [ ] 4.4 Open a new zsh session and confirm the powerlevel10k theme loads correctly.

## 5. Documentation and cleanup

- [ ] 5.1 Update `README.org` managed files table to include the powerlevel10k theme path.
