#!/bin/bash

# Generate kitty keybindings based on whether tmux/byobu is running

if [ -n "$TMUX" ]; then
    # Inside tmux/byobu - send commands to tmux
    cat <<'EOF'
# Tmux/Byobu mode keybindings
map super+1 send_text all \u00011
map super+2 send_text all \u00012
map super+3 send_text all \u00013
map super+4 send_text all \u00014
map super+5 send_text all \u00015
map super+6 send_text all \u00016
map super+7 send_text all \u00017
map super+8 send_text all \u00018
map super+9 send_text all \u00019
map super+0 send_text all \u00010
map super+t send_text all \u0001c
EOF
else
    # Not in tmux - use kitty native features
    cat <<'EOF'
# Kitty native mode keybindings
map super+1 goto_tab 1
map super+2 goto_tab 2
map super+3 goto_tab 3
map super+4 goto_tab 4
map super+5 goto_tab 5
map super+6 goto_tab 6
map super+7 goto_tab 7
map super+8 goto_tab 8
map super+9 goto_tab 9
map super+0 goto_tab 10
map super+t new_tab
EOF
fi
