#!/usr/bin/env sh

function toggle () {
    eww active-windows | grep -q calendar && eww close calendar || eww open calendar
}

# Execute accordingly
if [[ "$1" == "--toggle" ]]; then toggle
fi
