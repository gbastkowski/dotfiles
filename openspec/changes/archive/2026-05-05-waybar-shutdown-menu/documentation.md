# Waybar Shutdown Menu Setup

## Overview
Added shutdown menu functionality to Waybar, providing quick access to power management options.

## Installation
1. Copy `shutdown-menu.sh` to `~/.config/waybar/scripts/`
2. Make it executable: `chmod +x ~/.config/waybar/scripts/shutdown-menu.sh`
3. Add the custom module to your Waybar config

## Configuration
```json
"custom/shutdown": {
  "exec": "$HOME/.config/waybar/scripts/shutdown-menu.sh --show-menu",
  "return-type": "json",
  "format": "",
  "tooltip": "Power Menu",
  "on-click": "$HOME/.config/waybar/scripts/shutdown-menu.sh --show-menu"
}
```

## Dependencies
- rofi (for menu display)
- systemd (for power management)
- hyprland (for logout functionality)

## Usage
Click the power icon () in Waybar to show the menu with options:
- Shutdown
- Reboot
- Suspend
- Logout
- Cancel

## Customization
Edit `shutdown-menu.sh` to:
- Change menu appearance
- Add/remove options
- Modify icons or styling
- Adjust confirmation behavior