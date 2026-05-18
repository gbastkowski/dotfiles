#!/usr/bin/env bash
#
# Bootstrap ~/.m2/settings-security.xml from pass.
# Idempotent: overwrites the file each run, mode 0600.
#
# Requirements:
#   - pass entry: ista/mvn-master (the maven master password)
#   - mvn on PATH (to encrypt the master)
set -euo pipefail

target="$HOME/.m2/settings-security.xml"
mkdir -p "$(dirname "$target")"

master=$(pass show ista/mvn-master)
master_enc=$(mvn -emp "$master" 2>/dev/null | tail -1)

cat > "$target" <<EOF
<settingsSecurity>
  <master>${master_enc}</master>
</settingsSecurity>
EOF
chmod 600 "$target"
echo "Wrote $target"
