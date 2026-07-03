#!/usr/bin/env bash
# Claude Code PostToolUse hook: record Bash-tool commands into atuin history.
#
# Claude Code runs commands through its own Bash tool, not an interactive shell,
# so they never reach atuin's shell hooks. This script bridges the gap: it reads
# the PostToolUse JSON payload on stdin, extracts the command and its exit code,
# and writes a history entry tagged with `--author claude`.
#
# This is the write side of the atuin integration: the mcp-atuin MCP server reads
# history back (search_history, get_stats, ...), this hook feeds Claude's own
# commands in. home-manager deploys it to ~/.claude/hooks/ (see claude.nix); the
# PostToolUse hook in claude/settings.json points at it. Failures are swallowed
# so a hook error never blocks Claude.

set -uo pipefail

command -v atuin >/dev/null 2>&1 || exit 0
command -v jq    >/dev/null 2>&1 || exit 0

payload="$(cat)"

cmd="$(printf '%s' "$payload" | jq -r '.tool_input.command // empty')"
[ -z "$cmd" ] && exit 0

# cwd: prefer the payload's cwd, fall back to the process cwd.
cwd="$(printf '%s' "$payload" | jq -r '.cwd // empty')"
[ -n "$cwd" ] && cd "$cwd" 2>/dev/null

# Exit code: PostToolUse payload carries the raw exit status. Fall back to the
# interrupted flag, then to success, if the field is missing on this version.
exit_code="$(printf '%s' "$payload" | jq -r '.exit_code // empty')"
if [ -z "$exit_code" ]; then
  interrupted="$(printf '%s' "$payload" | jq -r '.tool_response.interrupted // false')"
  [ "$interrupted" = "true" ] && exit_code=130 || exit_code=0
fi

# atuin search/list need ATUIN_SESSION; start/end tolerate its absence but set
# one anyway so entries share a synthetic session per hook invocation.
export ATUIN_SESSION="${ATUIN_SESSION:-$(atuin uuid 2>/dev/null)}"

id="$(atuin history start --author claude "$cmd" 2>/dev/null)" || exit 0
[ -z "$id" ] && exit 0
atuin history end --exit "$exit_code" "$id" >/dev/null 2>&1 || true

exit 0
