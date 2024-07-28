#!/usr/bin/bash

function current_task() {
    result=$(emacsclient -e '(if org-clock-current-task (substring-no-properties org-clock-current-task) "")')
    if [[ "$result" == "\"\"" ]]; then
        icon=""
        time="\"\""
        expired="false"
    else
        icon="󰔛"
        time="$(emacsclient -e '(gunnar/get-clocked-time)')"
        if [[ "$(emacsclient -e '(gunnar/org-clock-expired-p)')" == "t" ]]
        then expired="true"
        else expired="false"
        fi
    fi
    echo "{ \"text\" : $result, \"icon\" : \"$icon\", \"time\" : $time, \"expired\" : $expired }"
}

if [[ "$1" == "--current-task" ]]; then current_task
fi
