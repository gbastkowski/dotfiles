#!/bin/bash
# Claude Code status line - Powerlevel10k rainbow style with powerline separators

input=$(cat)

# Extract basic info from input
MODEL=$(echo "$input" | jq -r '.model.display_name')
CWD=$(echo "$input" | jq -r '.workspace.current_dir')
COST=$(echo "$input" | jq -r '.cost.total_cost_usd')

# Powerline separators
SEP_RIGHT=$'\uE0B0'  #
SEP_LEFT=$'\uE0B2'   #

# Color definitions (matching p10k rainbow style)
# Format: \033[38;5;{fg};48;5;{bg}m
BG_DIR=4             # Blue background for directory
FG_DIR=254           # White foreground
BG_GIT=2             # Green background for clean git
BG_GIT_DIRTY=3       # Yellow background for dirty git
FG_GIT=0             # Black foreground
BG_MODEL=5           # Magenta background for model
FG_MODEL=254         # White foreground
BG_COST=6            # Cyan background for cost
FG_COST=0            # Black foreground

RESET='\033[0m'

# Get directory name
DIR_NAME="${CWD##*/}"

# Build directory segment
output="\033[38;5;${FG_DIR};48;5;${BG_DIR}m ${DIR_NAME} "

# Git segment
if git -C "$CWD" rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git -C "$CWD" branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        GIT_STATUS=""
        GIT_BG=$BG_GIT

        # Check for changes
        if ! git -C "$CWD" diff --quiet 2>/dev/null || \
           ! git -C "$CWD" diff --cached --quiet 2>/dev/null || \
           [ -n "$(git -C "$CWD" ls-files --others --exclude-standard 2>/dev/null)" ]; then
            GIT_BG=$BG_GIT_DIRTY
        fi

        # Add status indicators
        if ! git -C "$CWD" diff --quiet 2>/dev/null; then
            GIT_STATUS="${GIT_STATUS}!"
        fi
        if ! git -C "$CWD" diff --cached --quiet 2>/dev/null; then
            GIT_STATUS="${GIT_STATUS}+"
        fi
        if [ -n "$(git -C "$CWD" ls-files --others --exclude-standard 2>/dev/null)" ]; then
            GIT_STATUS="${GIT_STATUS}?"
        fi

        # Separator + git segment
        output="${output}\033[38;5;${BG_DIR};48;5;${GIT_BG}m${SEP_RIGHT}"
        output="${output}\033[38;5;${FG_GIT};48;5;${GIT_BG}m  ${BRANCH}"
        [ -n "$GIT_STATUS" ] && output="${output} ${GIT_STATUS}"
        output="${output} "

        PREV_BG=$GIT_BG
    else
        PREV_BG=$BG_DIR
    fi
else
    PREV_BG=$BG_DIR
fi

# Model segment
output="${output}\033[38;5;${PREV_BG};48;5;${BG_MODEL}m${SEP_RIGHT}"
output="${output}\033[38;5;${FG_MODEL};48;5;${BG_MODEL}m ${MODEL} "

# Cost segment
output="${output}\033[38;5;${BG_MODEL};48;5;${BG_COST}m${SEP_RIGHT}"
output="${output}\033[38;5;${FG_COST};48;5;${BG_COST}m \$$(printf '%.2f' $COST) "

# Final separator to reset
output="${output}\033[38;5;${BG_COST}m${SEP_RIGHT}${RESET}"

echo -e "$output"
