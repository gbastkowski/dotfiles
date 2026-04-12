# Shutdown Menu Design

## UI Design
- Button in Waybar right section with power icon ()
- Dropdown menu with 5 options
- Icons for each option:
  - Shutdown: 󰐥
  - Reboot: 󰜉
  - Suspend: 󰒲
  - Logout: 󰍃
  - Cancel: 󰅖

## Implementation Approach
1. Create custom Waybar module using GTK menu
2. Use existing systemd/logind DBus interface
3. Reuse logic from EWW shutdown.sh script
4. Style to match existing Waybar theme

## Files to Create
1. `~/.config/waybar/scripts/shutdown-menu.sh` - Main script
2. `~/.config/waybar/modules/shutdown.json` - Waybar module config
3. Update main Waybar config to include shutdown module

## Dependencies
- systemd/logind
- jgmenu or rofi for menu display
- Existing power management scripts