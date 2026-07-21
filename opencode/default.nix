{ pkgs, ... }:
let
  opencode = "/opt/homebrew/bin/opencode";
  host = "127.0.0.1";
  port = "4096";
  # Read the basic-auth password from pass at start time, then exec the
  # server. A login shell gives pass/gpg-agent the environment they need.
  serveScript = pkgs.writeShellScript "opencode-serve" ''
    export OPENCODE_SERVER_PASSWORD="$(${pkgs.pass}/bin/pass show private/opencode/server-password)"
    exec ${opencode} serve --hostname ${host} --port ${port}
  '';
in
{
  home.file.".config/opencode/opencode.json".source = ./opencode.json;
  home.file.".config/opencode/oh-my-openagent.json".source = ./oh-my-openagent.json;

  # On-demand opencode server: loaded at login but not started (no RunAtLoad,
  # no KeepAlive). Emacs starts it with `launchctl kickstart`
  # (opencode-client-launchd-label = "org.nix-community.home.opencode-serve"),
  # so the server is owned by launchd and survives Emacs restarts.
  launchd.agents.opencode-serve = {
    enable = true;
    config = {
      ProgramArguments = [ "/bin/sh" "-lc" "${serveScript}" ];
      RunAtLoad = false;
      KeepAlive = false;
      EnvironmentVariables = {
        OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
        OPENCODE_EXPERIMENTAL_OXFMT = "true";
        OPENCODE_EXPERIMENTAL_PLAN_MODE = "true";
        OPENCODE_EXPERIMENTAL_FILEWATCHER = "true";
        OPENCODE_EXPERIMENTAL_ICON_DISCOVERY = "true";
      };
      StandardOutPath = "/tmp/opencode-serve.log";
      StandardErrorPath = "/tmp/opencode-serve.err";
    };
  };
}
