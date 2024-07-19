#!/usr/bin/env sh

source ~/.config/eww/modules/common/eww.sh

function poweroff () {
    systemctl poweroff
}

function quit () {
    exit 0
}

function reboot () {
    systemctl reboot
}

function suspend () {
    systemctl suspend
}

toggle shutdown

if   [[ "$1" == "--poweroff" ]]; then poweroff
elif [[ "$1" == "--quit"     ]]; then quit
elif [[ "$1" == "--reboot"   ]]; then reboot
elif [[ "$1" == "--suspend"  ]]; then suspend
fi
