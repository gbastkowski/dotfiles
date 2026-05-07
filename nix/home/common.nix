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

  home.file.".local/bin/checkmail.sh"        = { source = ../../bin/checkmail.sh;        executable = true; };
  home.file.".local/bin/eos-capture-video.sh"= { source = ../../bin/eos-capture-video.sh;executable = true; };
  home.file.".local/bin/install-xmlls.sh"    = { source = ../../bin/install-xmlls.sh;    executable = true; };
  home.file.".local/bin/linux.include.sh"    = { source = ../../bin/linux.include.sh;    executable = true; };
  home.file.".local/bin/macos.include.sh"    = { source = ../../bin/macos.include.sh;    executable = true; };
  home.file.".local/bin/ollama-coder.sh"     = { source = ../../bin/ollama-coder.sh;     executable = true; };
  home.file.".local/bin/ollama-install.sh"   = { source = ../../bin/ollama-install.sh;   executable = true; };
  home.file.".local/bin/ostype.sh"           = { source = ../../bin/ostype.sh;           executable = true; };
  home.file.".local/bin/release.sh"          = { source = ../../bin/release.sh;          executable = true; };
  home.file.".local/bin/system-upgrade.sh"   = { source = ../../bin/system-upgrade.sh;   executable = true; };
  home.file.".local/bin/termux.include.sh"   = { source = ../../bin/termux.include.sh;   executable = true; };
  home.file.".local/bin/ensure-local-bin-in-path" = { source = ../../local/bin/ensure-local-bin-in-path; executable = true; };

  home.file.".byobu".source = ../../byobu/.byobu;
  home.file.".wgetrc".source = ../../wget/.wgetrc;
  home.file.".latexmkrc".source = ../../latexmk/.latexmkrc;

  home.file.".android-env".source = ../../home/.android-env;
  home.file.".ideavimrc".source = ../../idea/.ideavimrc;
  home.file.".uniteai.yml".source = ../../home/.uniteai.yml;
  home.file.".gnupg/gpg-agent.conf".source = ../../home/.gnupg/gpg-agent.conf;
  home.file.".gnupg/pinentry-wrapper" = {
    source = ../../home/.gnupg/pinentry-wrapper;
    executable = true;
  };

}
