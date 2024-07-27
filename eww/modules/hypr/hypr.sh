#!/usr/bin/env sh

function active_window () {
    hyprctl -j activewindow | jq -r '.title'
}

if   [[ "$1" == "--active-window" ]]; then active_window
fi
