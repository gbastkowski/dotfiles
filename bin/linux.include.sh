#!/usr/bin/env sh

upgrade_system_and_packages() {
    yay -Syu
}

upgrade_python_packages() {
    pipx upgrade-all
}
