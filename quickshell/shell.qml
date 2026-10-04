import Quickshell
import Quickshell.Hyprland
import Quickshell.Services.SystemTray
import QtQuick

// Minimal Hyprland bar: power + workspaces + centered clock, themed to
// match the waybar style.css (Catppuccin-mocha-ish: cream text, amber
// accents, translucent dark background).

Variants {
  id: root

  model: Quickshell.screens

  delegate: Component {
    PanelWindow {
      required property var modelData
      screen: modelData

      anchors {
        top: true
        left: true
        right: true
      }

      implicitHeight: 40
      color: Qt.rgba(51 / 255, 51 / 255, 63 / 255, 0.55)

      // Far left: power menu button (matches waybar `custom/shutdown`).
      Text {
        id: powerButton
        anchors {
          left: parent.left
          leftMargin: 12
          verticalCenter: parent.verticalCenter
        }
        text: "\uf011" //  nerd font power symbol
        color: "#efe3c8"
        font.family: "Ubuntu Nerd Font"
        font.pixelSize: 20
        font.weight: Font.Bold

        MouseArea {
          anchors.fill: parent
          onClicked: Quickshell.execDetached(["/home/gunnar/.local/bin/power-menu"])
        }
      }

      // Left: workspaces (after power button)
      Row {
        id: workspaceRow
        anchors {
          left: powerButton.right
          leftMargin: 10
          verticalCenter: parent.verticalCenter
        }
        spacing: 3

        Repeater {
          model: Hyprland.workspaces

          delegate: Rectangle {
            required property var modelData
            height: 28
            radius: 10
            implicitWidth: label.implicitWidth + 20 // horizontal padding

            // waybar: idle = transparent bg + #d8d0bf text;
            // active = amber gradient (#f0c674 -> #d89a43) + #161411 text;
            // urgent = #c65d4a bg + #fff5f0 text;
            // hover = #e0aa56 bg + #161411 text.
            property bool active: modelData.active
            property bool urgent: modelData.urgent
            property bool hovered: mouseArea.containsMouse

            gradient: active ? activeGradient : null
            color: urgent ? "#c65d4a" : (hovered ? "#e0aa56" : "transparent")

            Behavior on color {
              ColorAnimation { duration: 120 }
            }

            Gradient {
              id: activeGradient
              orientation: Gradient.Vertical
              GradientStop { position: 0.0; color: "#f0c674" }
              GradientStop { position: 1.0; color: "#d89a43" }
            }

            Text {
              id: label
              anchors.centerIn: parent
              text: modelData.name === "special:magic" ? "M" : modelData.name
              color: active || hovered ? "#161411" : (urgent ? "#fff5f0" : "#d8d0bf")
              font.family: "Ubuntu Nerd Font"
              font.pixelSize: 20
              font.bold: true
              font.weight: Font.Bold

              Behavior on color {
                ColorAnimation { duration: 120 }
              }
            }

            MouseArea {
              id: mouseArea
              anchors.fill: parent
              hoverEnabled: true
              onClicked: modelData.activate()
            }
          }
        }
      }

      // Centered clock, format "<time> | <date>"
      Text {
        id: clock
        anchors {
          horizontalCenter: parent.horizontalCenter
          verticalCenter: parent.verticalCenter
        }
        text: "--:-- | --/--"
        color: "#f4e7c2"
        font.family: "Ubuntu Nerd Font"
        font.pixelSize: 20
        font.bold: true
        font.weight: Font.Bold

        function update() {
          var now = new Date();
          clock.text = Qt.formatTime(now, "HH:mm") + " | " + Qt.formatDate(now, "MMM d");
        }

        Timer {
          interval: 1000
          running: true
          repeat: true
          onTriggered: clock.update()
          Component.onCompleted: clock.update()
        }
      }
      // Right: system tray
      Row {
        id: trayRow
        anchors {
          right: parent.right
          rightMargin: 12
          verticalCenter: parent.verticalCenter
        }
        spacing: 6

        Repeater {
          model: SystemTray.items ? SystemTray.items.values : []

          delegate: Item {
            required property var modelData
            width: 22
            height: 22

            Image {
              id: trayIcon
              anchors.centerIn: parent
              width: 20
              height: 20
              source: modelData.icon
              smooth: true
              visible: status === Image.Ready
            }

            Text {
              anchors.centerIn: parent
              text: "?"
              color: "#d8d0bf"
              font.family: "Ubuntu Nerd Font"
              font.pixelSize: 14
              visible: trayIcon.status !== Image.Ready
            }

            MouseArea {
              anchors.fill: parent
              acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
              onClicked: function (mouse) {
                if (!modelData) return
                if (mouse.button === Qt.LeftButton) modelData.activate()
                else if (mouse.button === Qt.MiddleButton) modelData.secondaryActivate()
              }
            }
          }
        }
      }
    }
  }
}
