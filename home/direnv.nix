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
  };
}
