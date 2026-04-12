#!/usr/bin/env bash
set -euo pipefail

source_script="$HOME/git/gbastkowski/dotfiles/eww/modules/emacs/emacs.sh"

if [[ ! -x "$source_script" ]]; then
  printf '{"text":"","tooltip":"Emacs task script unavailable","class":"hidden"}\n'
  exit 0
fi

payload="$(bash "$source_script" --current-task 2>/dev/null || true)"

if [[ -z "$payload" ]]; then
  printf '{"text":"","tooltip":"","class":"hidden"}\n'
  exit 0
fi

text="$(jq -r '.text // ""' <<<"$payload")"
icon="$(jq -r '.icon // ""' <<<"$payload")"
time_value="$(jq -r '.time // ""' <<<"$payload")"
expired="$(jq -r '.expired // false' <<<"$payload")"

if [[ -z "$text" ]]; then
  printf '{"text":"","tooltip":"","class":"hidden"}\n'
  exit 0
fi

display="$icon"
if [[ -n "$time_value" ]]; then
  display+="  $time_value"
fi
display+="  $text"

class="active"
if [[ "$expired" == "true" ]]; then
  class="expired"
fi

printf '{"text":"%s","tooltip":"%s\\nClocked: %s","class":"%s"}\n' \
  "$display" "$text" "$time_value" "$class"
