#!/usr/bin/env sh

upgrade_system_and_packages() {
    yay -Syu
    git -C ~/git/hyprwm/Hyprland pull
    make -C ~/git/hyprwm/Hyprland all
    sudo make -C ~/git/hyprwm/Hyprland install
    hyprpm update
}

upgrade_python_packages() {
    pipx upgrade-all
}
