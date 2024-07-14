#!/usr/bin/env sh

gh api notifications 2> /dev/null | jq '. | length'
