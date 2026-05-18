{ pkgs, ... }:
{
  # Minimal nix-darwin system config. Expand once trial activation succeeds.
  system.stateVersion = 4;
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Required so `darwin-rebuild` knows which user owns the home-manager profile.
  users.users."gunnar.bastkowski" = {
    name = "gunnar.bastkowski";
    home = "/Users/gunnar.bastkowski";
  };

  # Make sure flakes + the new nix command stay enabled at the system level.
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.zsh.enable = true;
}
