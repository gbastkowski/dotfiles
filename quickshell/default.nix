{ ... }:
{
  # Config only: on this host quickshell itself is installed via the Arch AUR
  # package (v0.3.0 at /usr/bin/quickshell), not via nixpkgs (which pins 0.2.1).
  home.file.".config/quickshell/shell.qml".source = ./shell.qml;
  home.file.".config/quickshell/ChatPanel.qml".source = ./ChatPanel.qml;
  # opencode config used by the chat widget's auto-started `opencode serve`
  home.file.".config/quickshell/ai/opencode.json".source = ./ai/opencode.json;
}
