#!/usr/bin/env bash
# Firefox for MCP: Marionette 2829, BiDi 9223. Close Firefox first.
set -euo pipefail

firefox=${FIREFOX_BINARY:-firefox}
if [[ -z ${FIREFOX_BINARY:-} && $(uname -s) == Darwin ]]; then
	firefox=/Applications/Firefox.app/Contents/MacOS/firefox
fi
export MOZ_MARIONETTE_PORT=2829
exec "$firefox" --marionette \
	--remote-debugging-port 9223 about:blank
