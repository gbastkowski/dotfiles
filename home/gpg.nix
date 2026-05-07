{ ... }:
{
  home.file.".gnupg/gpg-agent.conf".source = ../gnupg/gpg-agent.conf;
  home.file.".gnupg/pinentry-wrapper" = {
    source = ../gnupg/pinentry-wrapper;
    executable = true;
  };
}
