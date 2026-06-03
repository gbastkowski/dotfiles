#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

sudo cp "$repo_root/polkit/50-suspend.rules" /etc/polkit-1/rules.d/
sudo chmod 644 /etc/polkit-1/rules.d/50-suspend.rules

echo "Polkit rule installed."
echo "Restart polkit service: sudo systemctl restart polkit"
