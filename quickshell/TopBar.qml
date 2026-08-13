import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Services.SystemTray
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts

// Core top bar (one per screen via Variants): workspaces, window title,
// clock + weather (click opens the popups), and the system tray.
Scope {
    id: root

    signal calendarRequested()
    signal weatherRequested()

    Variants {
        model: Quickshell.screens

        PanelWindow {
            id: barWin
            required property var modelData
            readonly property string barScreen: modelData.name
            screen: modelData
            anchors { top: true; left: true; right: true }
            implicitHeight: 46
            exclusiveZone: 46
            color: "#bf33333f" // rgba(51,51,63,0.75) — waybar bg, less translucent

            SystemClock { id: clock; precision: SystemClock.Minutes }

            // compact weather fetch (Open-Meteo Berlin)
            Process {
                id: weatherProc
                running: false
                command: ["curl", "-s", "-m", "8",
                    "https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,weather_code&timezone=Europe%2FBerlin&forecast_days=1"]
                stdout: StdioCollector {
                    onStreamFinished: {
                        try {
                            var d = JSON.parse(this.text)
                            var c = d.current
                            var icon = "?"
                            var code = c.weather_code
                            if (code === 0) icon = "☀"
                            else if (code <= 3) icon = "◐"
                            else if (code >= 45 && code <= 48) icon = "≡"
                            else if (code >= 51 && code <= 67) icon = "≈"
                            else if (code >= 71 && code <= 86) icon = "❄"
                            else if (code >= 95) icon = "⚡"
                            weatherLabel.text = icon + " " + Math.round(c.temperature_2m) + "°"
                        } catch (e) {
                            weatherLabel.text = "☀ ?"
                        }
                    }
                }
            }

            Timer {
                interval: 30 * 60 * 1000
                running: true
                repeat: true
                onTriggered: { weatherProc.running = false; weatherProc.running = true }
            }

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 8
                anchors.rightMargin: 8
                spacing: 6

                // ---- left: workspaces + window title (fillWidth so the
                // center stays exactly centered regardless of content)
                Item {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 46
                    RowLayout {
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 4
                        Repeater {
                        model: Hyprland.workspaces
                        delegate: Rectangle {
                            required property var modelData
                            visible: modelData.monitor && modelData.monitor.name === barWin.barScreen
                            property bool hovered: false
                            width: 36
                            height: 32
                            radius: 10
                            color: hovered ? "#e0aa56" : "transparent"
                            gradient: (modelData.active && !hovered) ? activeGrad : null

                            Gradient {
                                id: activeGrad
                                GradientStop { position: 0.0; color: "#f0c674" }
                                GradientStop { position: 1.0; color: "#d89a43" }
                            }

                            Text {
                                anchors.centerIn: parent
                                text: modelData.id
                                color: (modelData.active || hovered) ? "#161411" : "#d8d0bf"
                                font.family: "Ubuntu Nerd Font"
                                font.pixelSize: 17
                                font.weight: Font.DemiBold
                            }
                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: hovered = true
                                onExited: hovered = false
                                onClicked: modelData.activate()
                            }
                        }
                    }

                        Text {
                            text: Hyprland.activeToplevel ? Hyprland.activeToplevel.title : ""
                            color: "#ffcf79"
                            font.family: "Ubuntu Nerd Font"
                            font.pixelSize: 16
                            font.weight: Font.DemiBold
                            elide: Text.ElideRight
                            Layout.preferredWidth: 240
                            Layout.leftMargin: 10
                            verticalAlignment: Text.AlignVCenter
                        }
                    }
                }

                // ---- center: clock + weather (click opens popups)
                Text {
                    text: Qt.formatDateTime(clock.date, "HH:mm  MMM d")
                    color: "#f4e7c2"
                    font.family: "Ubuntu Nerd Font"
                    font.pixelSize: 17
                    font.weight: Font.DemiBold
                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.calendarRequested()
                    }
                }

                Text {
                    id: weatherLabel
                    text: "☀ …"
                    color: "#f4e7c2"
                    font.family: "Ubuntu Nerd Font"
                    font.pixelSize: 17
                    font.weight: Font.DemiBold
                    Layout.leftMargin: 10
                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.weatherRequested()
                    }
                }

                Item { Layout.fillWidth: true }

                // ---- right: system tray (fillWidth to mirror the left)
                Item {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 46
                    RowLayout {
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 4
                        Repeater {
                            model: SystemTray.items
                            delegate: Item {
                                required property var modelData
                                width: 28
                                height: 28
                                IconImage {
                                    anchors.fill: parent
                                    anchors.margins: 3
                                    source: modelData.icon
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: {
                                        trayMenu.itemTitle = modelData.title || "(no title)"
                                        trayMenu.visible = !trayMenu.visible
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // tray item info panel (D-Bus menu rendering is a follow-up; shows the
    // item title for now). Anchored top-right, below the bar.
    PanelWindow {
        id: trayMenu
        property string itemTitle: ""
        visible: false
        implicitWidth: 220
        implicitHeight: 60
        anchors { top: true; right: true }
        margins { top: 44; right: 4 }
        exclusiveZone: 0
        color: "transparent"

        Rectangle {
            anchors.fill: parent
            color: "#f2111111"
            radius: 8
            border.width: 1
            border.color: "#333333"

            Text {
                anchors.fill: parent
                anchors.margins: 10
                text: trayMenu.itemTitle
                color: "#e8e8e8"
                font.pixelSize: 12
                wrapMode: Text.Wrap
                verticalAlignment: Text.AlignVCenter
            }
        }
    }
}
