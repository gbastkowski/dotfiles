import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import QtQuick
import QtQuick.Layouts
import "." as Local

// Left-edge sidebar pinned to the small HDMI monitor: system monitor on
// top, AI chat panel below. Reserves its width, pushing windows away.
// Also hosts calendar/weather popups, toggled by the waybar clock/weather
// via /tmp/qs-widget-trigger.
ShellRoot {
    id: root

    // Pin to HDMI-A-1 (the small screen); fall back to the primary monitor.
    readonly property var targetScreen: {
        for (var i = 0; i < Quickshell.screens.length; i++) {
            if (Quickshell.screens[i].name === "HDMI-A-1")
                return Quickshell.screens[i]
        }
        return Quickshell.screens.length > 0 ? Quickshell.screens[0] : null
    }

    // top bar (one per screen): workspaces, title, clock, weather, tray.
    // clock/weather clicks open the popups below.
    Local.TopBar {
        onCalendarRequested: root.toggleWidget("calendar")
        onWeatherRequested: root.toggleWidget("weather")
    }

    // SUPER+A focuses the chat input from anywhere
    GlobalShortcut {
        name: "focus-chat"
        description: "Focus the sidebar chat input"
        onPressed: chatPanel.focusInput()
    }

    PanelWindow {
        id: sidebarWin
        screen: root.targetScreen
        anchors { left: true; top: true; bottom: true }
        implicitWidth: 320
        exclusiveZone: width
        color: "transparent" // window buffer must carry alpha or hyprland won't blur
        focusable: true // needed so the chat input can receive keyboard input

        Rectangle {
            anchors.fill: parent
            color: "#c8111111" // 78% opaque: subtle translucency, wallpaper faintly visible

            ColumnLayout {
                anchors.fill: parent
                anchors.topMargin: 10
                anchors.bottomMargin: 8
                spacing: 6

                Local.SystemMonitor {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 330
                }

                Local.ChatPanel {
                    id: chatPanel
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                }
            }
        }

    }

    // ---- calendar + weather popups (toggled from waybar) ----------------
    // PopupWindow does not map on this stack; these are floating PanelWindows
    // anchored top-center of the HDMI monitor, just below the bar.
    PanelWindow {
        id: calendarPopup
        visible: false
        implicitWidth: 270
        implicitHeight: 330
        screen: root.targetScreen
        anchors { top: true; left: true }
        margins { top: 44; left: (sidebarWin.screen.width - implicitWidth) / 2 }
        exclusiveZone: 0
        color: "transparent"

        Rectangle {
            anchors.fill: parent
            color: "#dc111111"
            radius: 10
            border.width: 1
            border.color: "#333333"

            Local.CalendarWidget {
                anchors.fill: parent
                anchors.margins: 12
            }
        }
    }

    PanelWindow {
        id: weatherPopup
        visible: false
        implicitWidth: 250
        implicitHeight: 230
        screen: root.targetScreen
        anchors { top: true; left: true }
        margins { top: 44; left: (sidebarWin.screen.width - implicitWidth) / 2 + 150 }
        exclusiveZone: 0
        color: "transparent"

        Rectangle {
            anchors.fill: parent
            color: "#dc111111"
            radius: 10
            border.width: 1
            border.color: "#333333"

            Local.WeatherWidget {
                anchors.fill: parent
                anchors.margins: 12
            }
        }
    }

    // ---- trigger handling ----------------
    function toggleWidget(name) {
        if (name === "calendar") {
            if (calendarPopup.visible && !weatherPopup.visible) { calendarPopup.visible = false; return }
            calendarPopup.visible = true
            weatherPopup.visible = false
        } else if (name === "weather") {
            if (weatherPopup.visible && !calendarPopup.visible) { weatherPopup.visible = false; return }
            weatherPopup.visible = true
            calendarPopup.visible = false
        }
    }

    // waybar appends "calendar"/"weather" to this file on click; tail -f
    // streams the appended lines. Self-contained: truncates + touches first.
    Process {
        id: triggerTail
        running: true
        command: ["sh", "-c",
            ": > /tmp/qs-widget-trigger; exec tail -n 0 -f /tmp/qs-widget-trigger"]
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: function (line) {
                var t = line.trim()
                if (t === "calendar" || t === "weather") root.toggleWidget(t)
            }
        }
    }
}
