{ lib, ... }:
{
  home.username = "gunnar";
  home.homeDirectory = "/home/gunnar";

  home.file.".aliases".source = ../../home/.aliases;

  # Hyprland
  home.file.".config/hypr/hyprland.conf".source = ../../hypr/hyprland.conf;
  home.file.".config/hypr/hypridle.conf".source = ../../hypr/hypridle.conf;
  home.file.".config/hypr/hyprlock.conf".source = ../../hypr/hyprlock.conf;
  home.file.".config/hypr/hyprpaper.conf".source = ../../hypr/hyprpaper.conf;
  home.file.".config/hypr/screenshot.sh" = { source = ../../hypr/screenshot.sh; executable = true; };
  home.file.".config/hypr/switch-workspace.sh" = { source = ../../hypr/switch-workspace.sh; executable = true; };

  # Sway
  home.file.".config/sway/autostart".source = ../../sway/autostart;
  home.file.".config/sway/config".source = ../../sway/config;
  home.file.".config/sway/keyboard".source = ../../sway/keyboard;
  home.file.".config/sway/outputs".source = ../../sway/outputs;
  home.file.".config/sway/pointer".source = ../../sway/pointer;
  home.file.".config/sway/touchpad".source = ../../sway/touchpad;
  home.file.".config/sway/variables".source = ../../sway/variables;
  home.file.".config/sway/workspaces".source = ../../sway/workspaces;

  # Eww (widget system)
  home.file.".config/eww".source = ../../eww;

  # Walker (application launcher)
  home.file.".config/walker/config.toml".source = ../../walker/config.toml;
  home.file.".config/walker/themes/catppuccin.css".source = ../../walker/themes/catppuccin.css;
  home.file.".config/walker/themes/catppuccin.json".source = ../../walker/themes/catppuccin.json;
  home.file.".config/walker/themes/default.css".source = ../../walker/themes/default.css;
  home.file.".config/walker/themes/default.json".source = ../../walker/themes/default.json;
  home.file.".config/walker/themes/default.toml".source = ../../walker/themes/default.toml;
  home.file.".config/walker/themes/gunnar.css".source = ../../walker/themes/gunnar.css;
  home.file.".config/walker/themes/gunnar.json".source = ../../walker/themes/gunnar.json;
  home.file.".config/walker/themes/gunnar.toml".source = ../../walker/themes/gunnar.toml;

  # Wayvnc
  home.file.".config/wayvnc/config".source = ../../wayvnc/config;

  # Systemd user services
  home.file.".config/systemd/user/emacs.service.d/10-after-gfx.conf".source = ../../systemd/user/emacs.service.d/10-after-gfx.conf;
  home.file.".config/systemd/user/inhibit-suspend.service".source = ../../systemd/user/inhibit-suspend.service;
  home.file.".config/systemd/user/offlineimap-mu.service".source = ../../systemd/user/offlineimap-mu.service;
  home.file.".config/systemd/user/offlineimap-mu.timer".source = ../../systemd/user/offlineimap-mu.timer;
  home.file.".config/systemd/user/pgadmin4.service".source = ../../systemd/user/pgadmin4.service;
  home.file.".config/systemd/user/sway-headless.service".source = ../../systemd/user/sway-headless.service;

  programs.zsh.initContent = lib.mkMerge [
    (lib.mkOrder 500 ''
      # PATH additions (Linux-specific)
      [[ -d /usr/local/bin ]] && path_prepend "/usr/local/bin"
    '')
    (lib.mkAfter ''
      # nvm (system install)
      [[ -s "/usr/share/nvm/init-nvm.sh" ]] && source "/usr/share/nvm/init-nvm.sh"

      # Dart completions
      [[ -f /home/gunnar/.dart-cli-completion/zsh-config.zsh ]] && source /home/gunnar/.dart-cli-completion/zsh-config.zsh || true

      # Arch-specific
      case "$(uname -a)" in
        *arch*)
          [[ -n "$SSH_CONNECTION" ]] && systemctl --user start inhibit-suspend.service
          ;;
      esac
    '')
  ];
}
