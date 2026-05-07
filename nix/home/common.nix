{ pkgs, ... }:
{
  programs.home-manager.enable = true;

  home.stateVersion = "25.11";

  home.packages = with pkgs; [
    temurin-bin-21
    scala_3
    sbt
    nodejs_22
  ];

  home.file.".byobu".source = ../../byobu/.byobu;
  home.file.".wgetrc".source = ../../wget/.wgetrc;
  home.file.".latexmkrc".source = ../../latexmk/.latexmkrc;

  home.file.".android-env".source = ../../home/.android-env;
  home.file.".ideavimrc".source = ../../idea/.ideavimrc;
  home.file.".uniteai.yml".source = ../../home/.uniteai.yml;

}
