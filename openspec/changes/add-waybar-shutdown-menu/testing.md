# Testing Plan

## Manual Testing
1. Click power icon in Waybar - verify menu appears
2. Test each menu option:
   - Shutdown: Verify confirmation dialog appears
   - Reboot: Verify confirmation dialog appears  
   - Suspend: Verify system suspends
   - Logout: Verify Hyprland exits
   - Cancel: Verify menu closes without action

## Visual Testing
1. Check icon matches Waybar style
2. Verify tooltip appears on hover
3. Check menu styling matches theme
4. Verify proper spacing in modules-right

## Error Testing
1. Test without rofi installed
2. Test without systemd permissions
3. Verify graceful error handling

## Integration Testing
1. Test with existing Waybar modules
2. Verify no conflicts with other scripts
3. Check performance impact