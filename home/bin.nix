{ ... }:
{
  home.file.".local/bin/checkmail.sh"        = { source = ../bin/checkmail.sh;        executable = true; };
  home.file.".local/bin/eos-capture-video.sh"= { source = ../bin/eos-capture-video.sh;executable = true; };
  home.file.".local/bin/install-xmlls.sh"    = { source = ../bin/install-xmlls.sh;    executable = true; };
  home.file.".local/bin/ollama-coder.sh"     = { source = ../bin/ollama-coder.sh;     executable = true; };
  home.file.".local/bin/ollama-install.sh"   = { source = ../bin/ollama-install.sh;   executable = true; };
  home.file.".local/bin/release.sh"          = { source = ../bin/release.sh;          executable = true; };
  home.file.".local/bin/system-upgrade.sh"   = { source = ../bin/system-upgrade.sh;   executable = true; };
  home.file.".local/bin/ensure-local-bin-in-path" = { source = ../local/bin/ensure-local-bin-in-path; executable = true; };
}
