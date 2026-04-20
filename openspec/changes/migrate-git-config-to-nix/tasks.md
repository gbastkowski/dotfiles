## 1. Implement programs.git in the shared module

- [x] 1.1 Add `programs.git.enable = true` and user identity fields (`userName`, `userEmail`, `signing.key`) to `nix/home/common.nix`.
- [x] 1.2 Add pull, merge, push, init, advice, and github settings via `programs.git.extraConfig`.
- [x] 1.3 Enable `programs.git.lfs.enable = true` to replace the manual LFS filter block.
- [x] 1.4 Migrate all patterns from `git/.gitignore_global` into `programs.git.ignores`.

## 2. Collision handling and first activation

- [x] 2.1 Back up or move aside the existing `~/.gitconfig` on each target machine before the first switch.
- [x] 2.2 Run `home-manager build --flake ./nix#gunnar-dotfiles-darwin` to validate the configuration without activation.
- [x] 2.3 Run `home-manager switch --flake ./nix#gunnar-dotfiles-darwin` and confirm `~/.gitconfig` is a managed symlink.
- [x] 2.4 Verify git identity and LFS filter are active with `git config --list`.

## 3. Documentation

- [x] 3.1 Add a note to `README.org` that `~/.gitconfig` and `~/.gitignore_global` are now managed by Home Manager and document the move-aside step for first activation.

## 4. Cleanup

- [x] 4.1 Remove `git/.gitconfig` from the repository after validation is confirmed.
- [x] 4.2 Remove `git/.gitignore_global` from the repository after validation is confirmed.
