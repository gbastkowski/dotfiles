{ lib, ... }:
{
  # Config only: on this host quickshell itself is installed via the Arch AUR
  # package (v0.3.0 at /usr/bin/quickshell), not via nixpkgs (which pins 0.2.1).
  home.file.".config/quickshell/shell.qml".source = ./shell.qml;
  home.file.".config/quickshell/ChatPanel.qml".source = ./ChatPanel.qml;
  home.file.".config/quickshell/SystemMonitor.qml".source = ./SystemMonitor.qml;
  home.file.".config/quickshell/CalendarWidget.qml".source = ./CalendarWidget.qml;
  home.file.".config/quickshell/WeatherWidget.qml".source = ./WeatherWidget.qml;
  home.file.".config/quickshell/TopBar.qml".source = ./TopBar.qml;
  # opencode config used by the chat widget's auto-started `opencode serve`
  home.file.".config/quickshell/ai/opencode.json".source = ./ai/opencode.json;
  # isolated XDG_CONFIG_HOME so the server never merges ~/.config/opencode
  home.file.".config/quickshell/ai/xdg/.keep".text = "";

  # rangun delegate plugin. Copied (not symlinked) because opencode resolves
  # the ESM import from the file's real path; a nix-store symlink would miss
  # node_modules. Run `npm install` in the deployed dir once (node_modules is
  # machine-local, gitignored).
  home.activation.copyRangunPlugin = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p "$HOME/.config/quickshell/ai/plugins/rangun"
    install -m 644 ${./ai/plugins/rangun/index.js} "$HOME/.config/quickshell/ai/plugins/rangun/index.js"
    install -m 644 ${./ai/plugins/rangun/package.json} "$HOME/.config/quickshell/ai/plugins/rangun/package.json"
  '';
}
