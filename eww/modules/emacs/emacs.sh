#!/usr/bin/bash

function current_task() {
    result=$(emacsclient -e '(substring-no-properties org-clock-current-task)')
    if [ -n "$result" ]; then
        icon="\"󰔛\""
    else
        icon="\"\""
    fi
    echo "{ \"text\" : $result, \"icon\" : $icon}"
}

if   [[ "$1" == "--current-task" ]]; then current_task
fi
