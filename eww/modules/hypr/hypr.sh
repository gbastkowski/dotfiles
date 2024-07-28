#!/bin/bash

active_window () {
    hyprctl -j activewindow | jq -c '.'
}

active_workspace () {
    hyprctl -j activeworkspace | jq -c '.'
}

build_json () {
    echo "{ \"activewindow\" : $(active_window), \"activeworkspace\" : $(active_workspace) }"
}

listen() {
    SOCKET=UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock
    socat -U - $SOCKET | while read -r line; do build_json $line; done
}

if [[ "$1" == "--listen" ]]; then listen "$2"
else build_json
fi
