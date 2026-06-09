{ lib, pkgs, ... }:
{
  home.file = {
    ".byobu/backend".source      = ./byobu/backend;
    ".byobu/color".source        = ./byobu/color;
    ".byobu/color.tmux".source   = ./byobu/color.tmux;
    ".byobu/datetime.tmux".source = ./byobu/datetime.tmux;
    ".byobu/keybindings".source  = ./byobu/keybindings;
    ".byobu/keybindings.tmux".source = ./byobu/keybindings.tmux;
    ".byobu/profile".source      = ./byobu/profile;
    ".byobu/profile.tmux".source = ./byobu/profile.tmux;
    ".byobu/prompt".source       = ./byobu/prompt;
    ".byobu/status".source       = ./byobu/status;
    ".byobu/status-e".source     = ./byobu/status-e;
    ".byobu/statusrc".source     = ./byobu/statusrc;
    ".byobu/bin/hostname"        = { source = ./byobu/bin/hostname; executable = true; };
  };

  # Patch Homebrew byobu's stock tmuxrc to fix LP:#1871016 regression.
  # Lines 30-33 use tmux format strings #{BYOBU_ACCENT}/#{BYOBU_HIGHLIGHT}
  # that don't expand (they're shell vars, not tmux options), causing
  # "syntax error" on F5/startup. Replace with literal colours from
  # ~/.byobu/color.tmux. Idempotent; reapplies after every brew upgrade.
  home.activation.patchByobuTmuxrc = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    TMUXRC="/opt/homebrew/opt/byobu/share/byobu/profiles/tmuxrc"
    if [ -w "$TMUXRC" ] && grep -q '#{BYOBU_ACCENT}' "$TMUXRC"; then
      ${pkgs.gnused}/bin/sed -i.bak \
        -e 's|fg=#{BYOBU_ACCENT}|fg=#75507B|g' \
        -e 's|bg=#{BYOBU_HIGHLIGHT},fg=#{BYOBU_HIGHLIGHT}|bg=#DD4814,fg=#DD4814|g' \
        -e 's|display-panes-colour #{BYOBU_ACCENT}|display-panes-colour "#75507B"|g' \
        -e 's|display-panes-active-colour #{BYOBU_HIGHLIGHT}|display-panes-active-colour "#DD4814"|g' \
        "$TMUXRC"
      echo "byobu: patched $TMUXRC (LP:#1871016 workaround)"
    fi
  '';
}
