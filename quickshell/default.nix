{ ... }:
{
  home.file.".config/quickshell/shell.qml".source = ./shell.qml;
  home.file.".config/quickshell/scripts/system-status.sh" = {
    source = ./scripts/system-status.sh;
    executable = true;
  };
}
