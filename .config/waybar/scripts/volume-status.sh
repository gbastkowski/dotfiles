#!/usr/bin/env bash
set -euo pipefail

volume_line="$(wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null || true)"

if [[ -z "$volume_line" ]]; then
  printf '{"text":"󰖁","tooltip":"No default audio sink","class":"unavailable"}\n'
  exit 0
fi

muted=false
if [[ "$volume_line" == *"[MUTED]"* ]]; then
  muted=true
fi

volume_fraction="$(awk '{print $2}' <<<"$volume_line")"
percent="$(awk -v value="$volume_fraction" 'BEGIN { printf "%d", (value * 100) + 0.5 }')"

if (( percent < 0 )); then
  percent=0
fi

icon=""
if $muted || (( percent == 0 )); then
  icon="󰝟"
elif (( percent < 34 )); then
  icon=""
elif (( percent < 67 )); then
  icon=""
fi

sink_name="$(pactl get-default-sink 2>/dev/null || printf 'unknown')"

if $muted; then
  class="muted"
else
  class="active"
fi

printf '{"text":"%s","tooltip":"Sink: %s\\nVolume: %d%%\\nReported by wpctl: %s","class":"%s"}\n' \
  "$icon" "$sink_name" "$percent" "$volume_line" "$class"
