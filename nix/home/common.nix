{ ... }:
{
  programs.home-manager.enable = true;

  home.stateVersion = "25.11";

  home.file.".p10k.zsh".source = ../../zsh/.p10k.zsh;
}
