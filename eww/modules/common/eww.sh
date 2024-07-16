#!/usr/bin/env sh

function toggle () {
    eww active-windows | grep -q $1 && eww close $1 || eww open $1
}

if   [[ "$1" == "--toggle" ]]; then toggle $2
fi
