# Implementation

## Files Created
1. `~/.config/waybar/scripts/shutdown-menu.sh` - Main shutdown menu script
2. Updated `~/.config/waybar/config.jsonc` - Added shutdown module

## Script Details
- Uses rofi for menu display (consistent with existing setup)
- Supports 5 actions: Shutdown, Reboot, Suspend, Logout, Cancel
- Integrates with systemd for power management
- Uses hyprctl for Hyprland logout

## Waybar Configuration
- Added "custom/shutdown" module to modules-right
- Uses power icon ()
- Shows tooltip "Power Menu"
- Triggers script on click

## Testing Required
- Verify menu appears when clicking power icon
- Test each action (except actual shutdown/reboot)
- Check visual consistency with Waybar theme
- Test error handling