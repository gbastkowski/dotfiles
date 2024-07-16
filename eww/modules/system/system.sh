#!/usr/bin/env sh

function get_fan_speeds() {
    sensors -j 2> /dev/null | \
        jq '.["nct6798-isa-02a0"] | .. | objects | with_entries(select(.key | test("^fan.*_input$")))' | \
        jq -s 'add | with_entries(.key |= sub("_input$"; "") | .value |= floor)'
}

function get_memory_percentage() {
    printf "%.0f\n" $(free -m | grep Mem | awk '{print ($3/$2)*100}')
}

function get_memory_info() {
    total="$(free -m | grep Mem: | awk '{ print $2 }')"
    used="$(free -m | grep Mem: | awk '{ print $3 }')"

    if   [ "$1" = "total" ]; then echo $total
    elif [ "$1" = "used"  ]; then echo $used
    elif [ "$1" = "free"  ]; then echo $(expr $total - $used)
    fi
}

# Execute accordingly
if   [[ "$1" == "--toggle" ]]; then toggle
elif [[ "$1" == "--get-fan-speeds" ]];        then get_fan_speeds
elif [[ "$1" == "--get-memory-percentage" ]]; then get_memory_percentage
elif [[ "$1" == "--get-memory-info" ]];       then get_memory_info $2
fi
