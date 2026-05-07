{ ... }:
{
  home.file.".gnupg/gpg-agent.conf".source = ../home/.gnupg/gpg-agent.conf;
  home.file.".gnupg/pinentry-wrapper" = {
    source = ../home/.gnupg/pinentry-wrapper;
    executable = true;
  };
}
