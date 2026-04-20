## 1. Implement home.file entries in the shared module

- [x] 1.1 Add `home.file` entries for `~/.sbt/1.0/build.sbt`, `~/.sbt/1.0/plugins/plugins.sbt`, `~/.sbt/1.0/credentials.sbt`, and `~/.sbt/repositories` to `nix/home/common.nix`.

## 2. Collision handling and first activation

- [x] 2.1 Move aside existing `~/.sbt/1.0/build.sbt`, `~/.sbt/1.0/plugins/plugins.sbt`, `~/.sbt/1.0/credentials.sbt`, and `~/.sbt/repositories` on each target machine.
- [x] 2.2 Run `home-manager build --flake ./nix#gunnar-dotfiles-darwin` to validate without activation.
- [x] 2.3 Run `home-manager switch --flake ./nix#gunnar-dotfiles-darwin` and confirm the three files are managed symlinks.
- [x] 2.4 Verify sbt loads correctly and Artifactory credentials resolve via `pass`.

## 3. Documentation

- [x] 3.1 Update `README.org` managed files table to include the three sbt config files.

## 4. Cleanup

- [x] 4.1 Remove `sbt/.sbt/1.0/build.sbt` from the repository after validation.
- [x] 4.2 Remove `sbt/.sbt/1.0/plugins/plugins.sbt` from the repository after validation.
- [x] 4.3 Remove `sbt/.sbt/1.0/credentials.sbt` from the repository after validation.
- [x] 4.4 Remove `sbt/.sbt/repositories` from the repository after validation.
