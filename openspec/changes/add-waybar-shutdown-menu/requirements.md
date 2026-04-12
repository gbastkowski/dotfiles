# Shutdown Menu Requirements

## Functional Requirements
1. Shutdown menu should appear when clicking a dedicated button in Waybar
2. Menu should include options for:
   - Shutdown
   - Reboot
   - Suspend
   - Logout
   - Cancel
3. Menu should be visually consistent with existing Waybar style
4. Menu should work with Hyprland window manager

## Technical Requirements
1. Implement as a custom Waybar module
2. Use existing EWW shutdown scripts as reference
3. Integrate with systemd/logind for power management
4. Ensure proper error handling and user feedback

## UI Requirements
1. Menu should appear near the Waybar button
2. Use consistent icons and styling
3. Provide visual feedback on hover
4. Clear confirmation for destructive actions