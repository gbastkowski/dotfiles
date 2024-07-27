#!/usr/bin/bash

function current_task() {
    result=$(emacsclient -e '(substring-no-properties org-clock-current-task)')
    echo $result | sed 's/^"//;s/"$//'
}

if   [[ "$1" == "--current-task" ]]; then current_task
fi
