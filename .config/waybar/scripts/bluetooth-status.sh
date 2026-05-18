#!/usr/bin/env bash

get_icon() {
    if [ "$1" = "on" ]; then
        echo "󰂯"
    else
        echo "󰂲"
    fi
}

get_status() {
    if ! bluetoothctl show >/dev/null 2>&1; then
        echo "off"
        return
    fi

    powered=$(bluetoothctl show 2>/dev/null | grep "Powered:" | awk '{print $2}')
    if [ "$powered" != "yes" ]; then
        echo "off"
        return
    fi

    connected=$(bluetoothctl devices Connected 2>/dev/null | wc -l)
    if [ "$connected" -gt 0 ]; then
        echo "connected"
    else
        echo "on"
    fi
}

get_tooltip() {
    status=$(get_status)

    if [ "$status" = "off" ]; then
        echo "Bluetooth disabled"
        return
    fi

    adapter=$(bluetoothctl show 2>/dev/null | grep "Name:" | cut -d: -f2 | xargs)
    tooltip="Adapter: ${adapter}"

    devices=$(bluetoothctl devices Connected 2>/dev/null | cut -d' ' -f3-)
    if [ -n "$devices" ]; then
        tooltip="${tooltip} Connected: ${devices}"
    fi

    echo "$tooltip"
}

toggle_bluetooth() {
    status=$(get_status)
    if [ "$status" = "off" ]; then
        bluetoothctl power on >/dev/null 2>&1
    else
        bluetoothctl power off >/dev/null 2>&1
    fi
}

case "$1" in
    --toggle)
        toggle_bluetooth
        ;;
    *)
        status=$(get_status)
        icon=$(get_icon "$status")
        tooltip=$(get_tooltip)

        class=""
        if [ "$status" = "connected" ]; then
            class="connected"
        elif [ "$status" = "off" ]; then
            class="disabled"
        fi

        echo "{\"text\":\"$icon\",\"tooltip\":\"$tooltip\",\"class\":\"$class\"}"
        ;;
esac
