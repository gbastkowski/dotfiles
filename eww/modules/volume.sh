#!/usr/bin/env sh

# Get Volume
get_volume() {
    status=`amixer get Master | tail -n1 | grep -wo 'on'`
    if [[ "$status" == "on" ]]
    then
        volume=`amixer get Master | tail -n1 | awk -F ' ' '{print $4}' | tr -d '[%]'`
        echo "$volume"
    else
        echo "Mute"
    fi
}

# Subscribe to volume changes
subscribe_volume() {
    get_volume

    # pactl subscribe | while read -r event
    alsactl monitor | while read -r event
    do
        echo $event
        if echo "$event" | grep -q "Playback"
        then get_volume
        fi
    done
}

# Get icons
get_icon() {
    vol="$(get_volume)"
    current="${vol%%%}"
    status=`amixer get Master | tail -n1 | grep -wo 'on'`

    if [[ "$status" == "on" ]]
    then
        if [[ "$current" -eq "0" ]]
        then echo "󰸈"
        elif [[ ("$current" -ge "0") && ("$current" -le "30") ]]
        then echo "󰕿"
        elif [[ ("$current" -ge "30") && ("$current" -le "80") ]]
        then echo "󰖀"
        elif [[ "$current" -ge "80" ]]
        then echo "󰕾"
        fi
    else echo "󰖁"
    fi
}

# Subscribe to change event and Get icons
subscribe_icon() {
    subscribe_volume | while read -r event
    do get_icon
    done
}

lower_volume() {
    amixer -Mq set Master,0 5%- unmute
}
raise_volume() {
    amixer -Mq set Master,0 5%+ unmute
}
# Toggle Mute
toggle_mute() {
    status=`amixer get Master | tail -n1 | grep -wo 'on'`

    if [[ "$status" == "on" ]]
    then amixer set Master toggle
    else amixer set Master toggle
    fi
}

# Execute accordingly
if [[ "$1" == "--get" ]];                 then get_volume
elif [[ "$1" == "--subscribe-volume" ]];  then subscribe_volume
elif [[ "$1" == "--subscribe-icon" ]];    then subscribe_icon
elif [[ "$1" == "--icon" ]];              then get_icon
elif [[ "$1" == "--inc" ]];               then raise_volume
elif [[ "$1" == "--dec" ]];               then lower_volume
elif [[ "$1" == "--toggle" ]];            then toggle_mute
else                                           get_volume
fi
