#!/usr/bin/env sh

source ~/.config/eww/modules/common/eww.sh

function poweroff () {
    toggle shutdown
    zenity --question --text="Do you really want to shutdown?" --default-cancel && \
        systemctl poweroff
}

function quit () {
    toggle shutdown
    zenity --question --text="Do you really want to quit Hyprland?" --default-cancel && \
        exit 0
}

function reboot () {
    toggle shutdown
    zenity --question --text="Do you really want to reboot?" --default-cancel && \
        systemctl reboot
}

function suspend () {
    toggle shutdown
    toggle confirm-suspend
}

function confirmsuspend () {
    toggle confirm-suspend
    systemctl suspend
}

if   [[ "$1" == "--poweroff" ]]; then poweroff
elif [[ "$1" == "--quit"     ]]; then quit
elif [[ "$1" == "--reboot"   ]]; then reboot
elif [[ "$1" == "--suspend"  ]]; then suspend
elif [[ "$1" == "--confirm-suspend"  ]]; then confirmsuspend
elif [[ "$1" == "--cancel"  ]]; then toggle confirm-suspend
fi
