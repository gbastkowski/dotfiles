#!/usr/bin/env sh

function toggle () {
    eww active-windows | grep -q system && eww close system || eww open system
}

function get_fan_speeds() {
    sensors -j 2> /dev/null | \
        jq '.["nct6798-isa-02a0"] | .. | objects | with_entries(select(.key | test("^fan.*_input$")))' | \
        jq -s 'add | with_entries(.key |= sub("_input$"; "") | .value |= floor)'
}

# Execute accordingly
if   [[ "$1" == "--toggle" ]]; then toggle
elif [[ "$1" == "--get-fan-speeds" ]]; then get_fan_speeds
fi
