#!/usr/bin/env bash
set -euo pipefail

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Android Emulator
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🤖
# @raycast.packageName Development

# Documentation:
# @raycast.description Boot an Android Virtual Device in the background
# @raycast.author Gunnar Bastkowski

sdk="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-$HOME/Library/Android/sdk}}"
emulator="$sdk/emulator/emulator"

if [ ! -x "$emulator" ]; then
  echo "emulator not found at $emulator"
  exit 1
fi

avd="${1:-}"
if [ -z "$avd" ]; then
  avd="$("$emulator" -list-avds | head -n 1)"
fi

if [ -z "$avd" ]; then
  echo "no AVD configured; create one in Android Studio"
  exit 1
fi

if "$sdk/platform-tools/adb" devices 2>/dev/null | grep -q '^emulator-'; then
  echo "emulator already running"
  exit 0
fi

nohup "$emulator" -avd "$avd" >/dev/null 2>&1 &
echo "Starting $avd"
