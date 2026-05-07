source $BYOBU_PREFIX/share/byobu/profiles/tmux

set -g allow-rename on
set -g automatic-rename on
set -g automatic-rename-format "#{b:pane_current_path} — #{pane_current_command}"
