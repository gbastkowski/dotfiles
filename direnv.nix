{ pkgs, ... }:
let
  systemDirenv = if pkgs.stdenv.isDarwin
    then pkgs.writeShellScriptBin "direnv" ''exec /opt/homebrew/bin/direnv "$@"''
    else pkgs.direnv;
in
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    package = systemDirenv;
    config.global = {
      # Only messages matching the filter are shown: drop "loading/export",
      # keep errors such as ".envrc is blocked". Needs direnv >= 2.36.
      # (log_format = "-" is documented but printed literally in 2.37.1.)
      log_filter = "^error";
      hide_env_diff = true;
    };
  };
}
