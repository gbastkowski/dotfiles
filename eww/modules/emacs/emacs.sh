#!/usr/bin/bash

function current_task() {
    result=$(emacsclient -e '(if org-clock-current-task (substring-no-properties org-clock-current-task) "")')
    if [[ "$result" == "\"\"" ]]; then
        icon=""
        time=""
    else
        icon="󰔛"
        time="[$(emacsclient -e '(org-clock-get-clocked-time)')]"
    fi
    echo "{ \"text\" : $result, \"icon\" : \"$icon\", \"time\" : \"$time\"}"
}

if [[ "$1" == "--current-task" ]]; then current_task
fi
