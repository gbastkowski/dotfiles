#!/usr/bin/env sh

# Get Volume
get_volume() {
    status=`amixer get Master | tail -n1 | grep -wo 'on'`
    if [[ "$status" == "on" ]]
    then
        volume=`amixer get Master | tail -n1 | awk -F ' ' '{print $4}' | tr -d '[]'`
        echo "$volume"
    else
        echo "Mute"
    fi
}

# Get icons
get_icon() {
    vol="$(get_volume)"
    current="${vol%%%}"
    status=`amixer get Master | tail -n1 | grep -wo 'on'`

    if [[ "$status" == "on" ]]; then
	if [[ "$current" -eq "0" ]]
        then
	    echo "icons/notification-audio-volume-muted.svg"
	elif [[ ("$current" -ge "0") && ("$current" -le "30") ]]
        then
	    echo "icons/notification-audio-volume-low.svg"
	elif [[ ("$current" -ge "30") && ("$current" -le "60") ]]
        then
	    echo "icons/notification-audio-volume-medium.svg"
	elif [[ ("$current" -ge "60") && ("$current" -le "100") ]]
        then
	    echo "icons/notification-audio-volume-high.svg"
	fi
    else
	echo "icons/notification-audio-volume-muted.svg"
    fi
}

# Increase Volume
inc_volume() {
    amixer -Mq set Master,0 5%+ unmute
}

# Decrease Volume
dec_volume() {
    amixer -Mq set Master,0 5%- unmute
}

# Toggle Mute
toggle_mute() {
    status=`amixer get Master | tail -n1 | grep -wo 'on'`

    if [[ "$status" == "on" ]]
    then
        amixer set Master toggle
    else
        amixer set Master toggle
    fi
}

# Execute accordingly
if [[ "$1" == "--get" ]]
then
    get_volume
elif [[ "$1" == "--icon" ]]
then
    get_icon
elif [[ "$1" == "--inc" ]]
then
    inc_volume
elif [[ "$1" == "--dec" ]]
then
    dec_volume
elif [[ "$1" == "--toggle" ]]
then
    toggle_mute
else
    get_volume
fi