source $BYOBU_PREFIX/share/byobu/profiles/tmux

# Window labels: name tabs after the directory, not the running process.
# Running claude/nvim/sbt everywhere makes process names useless as labels.
#
# allow-rename off  -> ignore OSC 0/2 title escapes from the shell and from
#                      tools like claude, which otherwise clobber the name.
# automatic-rename  -> tmux keeps the name in sync with automatic-rename-format.
set -g allow-rename off
set -g automatic-rename on
set -g automatic-rename-format "#{?#{==:#{b:pane_current_path},#{b:HOME}},~,#{b:pane_current_path}}"
