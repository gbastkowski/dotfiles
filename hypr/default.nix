{ ... }:
{
  home.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;
  home.file.".config/hypr/hypridle.conf".source = ./hypridle.conf;
  home.file.".config/hypr/hyprlock.conf".source = ./hyprlock.conf;
  home.file.".config/hypr/hyprpaper.conf".source = ./hyprpaper.conf;
  home.file.".config/hypr/screenshot.sh" = { source = ./screenshot.sh; executable = true; };
  home.file.".config/hypr/switch-workspace.sh" = { source = ./switch-workspace.sh; executable = true; };
}
