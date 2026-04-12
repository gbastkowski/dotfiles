#!/usr/bin/env bash
set -euo pipefail

weather_script="$HOME/git/gbastkowski/dotfiles/eww/modules/weather/weather.sh"

if [[ ! -x "$weather_script" ]]; then
  printf '{"text":"","tooltip":"Weather script unavailable","class":"unavailable"}\n'
  exit 0
fi

bash "$weather_script" --getdata >/dev/null 2>&1 || true

icon="$(bash "$weather_script" --icon 2>/dev/null | tr -d '\n' | sed 's/[[:space:]]*$//')"
temp="$(bash "$weather_script" --temp 2>/dev/null | tr -d '\n')"
stat="$(bash "$weather_script" --stat 2>/dev/null | tr -d '\n')"
name="$(bash "$weather_script" --name 2>/dev/null | tr -d '\n')"
humidity="$(bash "$weather_script" --humidity 2>/dev/null | tr -d '\n')"
wind_speed="$(bash "$weather_script" --wind-speed 2>/dev/null | tr -d '\n')"
wind_direction="$(bash "$weather_script" --wind-direction 2>/dev/null | tr -d '\n')"
sunrise="$(bash "$weather_script" --sunrise 2>/dev/null | tr -d '\n')"
sunset="$(bash "$weather_script" --sunset 2>/dev/null | tr -d '\n')"

if [[ -z "$icon" ]]; then
  icon=""
fi

if [[ -z "$temp" ]]; then
  temp="--"
fi

tooltip="$stat"
if [[ -n "$name" ]]; then
  tooltip+="\\n$name"
fi
tooltip+="\\nTemp: $temp"
if [[ -n "$humidity" ]]; then
  tooltip+="\\nHumidity: $humidity"
fi
if [[ -n "$wind_speed" || -n "$wind_direction" ]]; then
  tooltip+="\\nWind: $wind_direction $wind_speed"
fi
if [[ -n "$sunrise" || -n "$sunset" ]]; then
  tooltip+="\\nSun: $sunrise / $sunset"
fi

printf '{"text":"%s  %s","tooltip":"%s","class":"active"}\n' "$icon" "$temp" "$tooltip"
