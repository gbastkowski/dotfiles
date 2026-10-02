{ config, pkgs, lib, ... }:
{
  # herdr is installed by the system's package manager (Homebrew on macOS, the distro's on Linux);
  # home-manager only manages its config and the headless server unit below.
  #
  # Note: herdr writes `onboarding = false` into this file after first-run setup,
  # and `herdr config reset-keys` rewrites it too.
  # Both would fail against a read-only nix symlink, so the setting is kept here explicitly
  # and keybindings are managed in git rather than via reset-keys.
  #
  # Apply changes to a running server with `herdr server reload-config`.
  #
  # @HOME@ in config.toml is replaced with home.homeDirectory so the sound
  # paths work on every host.
  home.file.".config/herdr/config.toml".text =
    builtins.replaceStrings [ "@HOME@" ] [ config.home.homeDirectory ]
      (builtins.readFile ./herdr/config.toml);

  # Notification sounds. config.toml resolves these by absolute path to avoid
  # relative-path ambiguity against the nix-store symlink target.
  home.file.".config/herdr/sounds".source = ./herdr/sounds;

  # Persistent headless server, independent of Hyprland. logind lingering is
  # enabled for gunnar, so WantedBy=default.target starts it at boot and it
  # keeps running (and stays reachable over `herdr --remote`) even when no
  # one is logged in. Panes inherit this unit's environment, so per-session
  # Hyprland variables are intentionally absent: tooling that needs them must
  # resolve them at call time (see bin/system-upgrade.sh).
  systemd.user.services.herdr = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "herdr terminal workspace server";
      Documentation = [ "https://herdr.dev" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "/usr/bin/herdr server";
      Restart = "on-failure";
      RestartSec = 3;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
