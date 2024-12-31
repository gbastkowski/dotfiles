#!/usr/bin/env sh

upgrade_system_and_packages() {
    pkg update
    pkg upgrade
}

upgrade_python_packages() {
    echo "no python to upgrade"
}
