#!/usr/bin/env bash
set -euo pipefail

# Waybar Shutdown Menu Script
# Based on EWW shutdown.sh but adapted for Waybar

function show_menu() {
    # Use a simple terminal-based menu as fallback
    echo "Power Menu:"
    echo "1. Shutdown"
    echo "2. Reboot"
    echo "3. Suspend"
    echo "4. Logout"
    echo "5. Cancel"
    
    read -p "Select option (1-5): " choice
    
    case "$choice" in
        1|"1")
            systemctl poweroff
            ;;
        2|"2")
            systemctl reboot
            ;;
        3|"3")
            systemctl suspend
            ;;
        4|"4")
            hyprctl dispatch exit
            ;;
        5|"5"|"")
            exit 0
            ;;
        *)
            echo "Invalid option"
            exit 1
            ;;
    esac
}

# Main execution
if [[ "$1" == "--show-menu" ]]; then
    # Try to use terminal if available, otherwise use zenity
    if [[ -t 1 ]]; then
        show_menu
    else
        # Fallback to zenity if no terminal
        choice=$(zenity --list --column="Option" --column="Action" \
            "1" "Shutdown" \
            "2" "Reboot" \
            "3" "Suspend" \
            "4" "Logout" \
            "5" "Cancel" \
            --title="Power Menu" --width=200 --height=250 --print-column=2)
        
        case "$choice" in
            "Shutdown") systemctl poweroff ;;
            "Reboot") systemctl reboot ;;
            "Suspend") systemctl suspend ;;
            "Logout") hyprctl dispatch exit ;;
            "Cancel"|"") exit 0 ;;
        esac
    fi
fi