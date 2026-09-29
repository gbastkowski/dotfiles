{ ... }:
{
  # herdr is installed via Homebrew; home-manager only manages its config.
  #
  # Note: herdr writes `onboarding = false` into this file after first-run
  # setup, and `herdr config reset-keys` rewrites it too. Both would fail
  # against a read-only nix symlink, so the setting is kept here explicitly
  # and keybindings are managed in git rather than via reset-keys.
  #
  # Apply changes to a running server with `herdr server reload-config`.
  home.file.".config/herdr/config.toml".source = ./herdr/config.toml;
}
