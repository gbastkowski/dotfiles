#!/usr/bin/env sh

switch_workspace () {
    hyprctl dispatch workspace $1
    eww update active-workspace=`hyprctl -j activeworkspace | jq -r '.name'`
}

handle_hyprland_event() {
    case $1 in
        workspacev2*)
            echo "${1#*>>}" | awk -F ',' '{print $1}'
            ;;
    esac
}

listen_hyprland_events() {
    SOCKET=UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock
    socat -U - $SOCKET | while read -r line; do handle_hyprland_event "$line"; done
}

listen_hyprland_events
