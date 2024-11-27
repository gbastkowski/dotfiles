#!/usr/bin/env sh

upgrade_system_and_packages() {
    yay -Syu
    make -C ~/git/hyprwm/Hyprland all
    sudo make -C ~/git/hyprwm/Hyprland install
}

upgrade_python_packages() {
    pipx upgrade-all
}
