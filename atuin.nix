{ ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;

    # Local-only: no cloud sync. Run `atuin login`/`atuin register` manually to add sync later.
    flags = [ "--disable-up-arrow" ];

    settings = {
      # Up arrow stays as normal history; only Ctrl-R opens atuin.
      enter_accept = false;
      style = "compact";
      inline_height = 20;
    };
  };
}
