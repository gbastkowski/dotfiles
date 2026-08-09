{ ... }:
{
  home.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;
  home.file.".config/hypr/hypridle.conf".source = ./hypridle.conf;
  home.file.".config/hypr/hyprlock.conf".source = ./hyprlock.conf;
  home.file.".config/hypr/hyprpaper.conf".source = ./hyprpaper.conf;
  home.file.".config/hypr/screenshot.sh" = { source = ./screenshot.sh; executable = true; };

  # Hermes QuickShell: chromeless Chromium window loading the Hermes WebUI
  # (Open WebUI on smarty, LAN). Launch via wofi/launcher, float via windowrule.
  xdg.desktopEntries.hermes = {
    name = "Hermes";
    comment = "Hermes WebUI (Open WebUI)";
    exec = "chromium --app=http://smarty:3000 --window-size=780,900";
    terminal = false;
    categories = [ "Utility" ];
  };
}
