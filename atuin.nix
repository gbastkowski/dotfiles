{ lib, ... }:
{
  # atuin is installed via Homebrew (newer than the nixpkgs pin, and it already
  # owns the on-disk history DB schema). home-manager only manages the config
  # file and the zsh init hook, pointing at whichever `atuin` is on PATH (brew).

  home.file.".config/atuin/config.toml".text = ''
    # Local-only: no cloud sync. Run `atuin login`/`atuin register` to add sync later.
    # Up arrow stays as normal history; only Ctrl-R opens atuin.
    enter_accept = false
    style = "compact"
    inline_height = 20
  '';

  programs.zsh.initContent = lib.mkAfter ''
    # atuin (Homebrew binary on PATH). --disable-up-arrow: only Ctrl-R opens search.
    if command -v atuin >/dev/null 2>&1; then
      eval "$(atuin init zsh --disable-up-arrow)"
    fi
  '';
}
