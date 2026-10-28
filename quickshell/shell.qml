import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
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
      id: bar
      required property var modelData
      screen: modelData
      readonly property HyprlandMonitor screenMonitor: Hyprland.monitorFor(screen)

      function cleanTitle(title) {
        if (!title) return ""
        title = title.replace(/ - Mozilla Firefox$/, "")
        title = title.replace(/ \u2014 Mozilla Firefox$/, "")
        title = title.replace(/ - Emacs$/, "")
        title = title.replace(/ \u2014 kitty$/, "")
        if (title.length > 64) title = title.substring(0, 64) + "..."
        return title
      }

      // Title of the focused window, only shown on the screen it lives on.
      readonly property string windowTitle: {
        var t = Hyprland.activeToplevel
        if (!t || !t.workspace || !t.workspace.monitor || !screenMonitor) return ""
        if (t.workspace.monitor.name !== screenMonitor.name) return ""
        return cleanTitle(t.title)
      }

      readonly property string btScript: Quickshell.env("HOME") + "/git/gbastkowski/dotfiles/.config/waybar/scripts/bluetooth-status.sh"
      readonly property string volScript: Quickshell.env("HOME") + "/git/gbastkowski/dotfiles/.config/waybar/scripts/volume-status.sh"
      readonly property string sysScript: Quickshell.env("HOME") + "/.config/quickshell/scripts/system-status.sh"

      anchors {
        top: true
        left: true
        right: true
      }

      implicitHeight: 40
      color: Qt.rgba(51 / 255, 51 / 255, 63 / 255, 0.55)

      // Far left: power menu button (matches waybar `custom/shutdown`).
      Rectangle {
        id: powerButton
        anchors {
          left: parent.left
          leftMargin: 12
          verticalCenter: parent.verticalCenter
        }
        height: 28
        width: powerIcon.implicitWidth + 28 // waybar padding: 0 14px
        radius: 10
        color: powerMouse.containsMouse ? "#e0aa56" : "transparent"

        Behavior on color {
          ColorAnimation { duration: 120 }
        }

        Text {
          id: powerIcon
          anchors.centerIn: parent
          text: "\uf011" //  nerd font power symbol
          color: powerMouse.containsMouse ? "#161411" : "#efe3c8"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
          font.weight: Font.Bold

          Behavior on color {
            ColorAnimation { duration: 120 }
          }
        }

        MouseArea {
          id: powerMouse
          anchors.fill: parent
          hoverEnabled: true
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
            visible: {
              if (modelData.monitor) {
                return screenMonitor && modelData.monitor.name === screenMonitor.name
              }
              // Special (scratchpad) workspaces have no monitor; show on the focused screen.
              return screenMonitor && screenMonitor.focused
            }
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

      // Current window title (matches waybar hyprland/window)
      Text {
        id: windowTitleLabel
        anchors {
          left: workspaceRow.right
          leftMargin: 12
          verticalCenter: parent.verticalCenter
        }
        text: windowTitle
        color: "#ffcf79"
        font.family: "Ubuntu Nerd Font"
        font.pixelSize: 20
        font.bold: true
        font.weight: Font.Bold
        elide: Text.ElideRight
        width: Math.min(implicitWidth, 420)
        maximumLineCount: 1
      }

      // Centered: clock + weather (matches waybar modules-center order)
      Row {
        id: centerRow
        anchors {
          horizontalCenter: parent.horizontalCenter
          verticalCenter: parent.verticalCenter
        }
        spacing: 32

        Text {
          id: clock
          text: "\uf017  --:-- | --/--"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
          font.bold: true
          font.weight: Font.Bold

          function update() {
            var now = new Date();
            clock.text = "\uf017  " + Qt.formatTime(now, "HH:mm") + " | " + Qt.formatDate(now, "MMM d");
          }

          Timer {
            interval: 1000
            running: true
            repeat: true
            onTriggered: clock.update()
            Component.onCompleted: clock.update()
          }
        }

        // Weather: icon + temp, refreshed every 15 minutes.
        Text {
          id: weatherLabel
          text: "\uf0c2  --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
          font.bold: true
          font.weight: Font.Bold

          MouseArea {
            id: weatherMouse
            anchors.fill: parent
            hoverEnabled: true
          }
        }
      }
      // Weather data: run the shared weather-status.sh and parse its JSON.
      Process {
        id: weatherProcess
        command: ["bash", Quickshell.env("HOME") + "/git/gbastkowski/dotfiles/.config/waybar/scripts/weather-status.sh"]
        stdout: StdioCollector {
          onStreamFinished: {
            var parsed = null
            try {
              parsed = JSON.parse(text)
            } catch (e) {}
            if (parsed && parsed.text) {
              weatherLabel.text = parsed.text
              weatherTooltipText.text = parsed.tooltip || ""
            } else {
              weatherLabel.text = "\uf0c2  --"
              weatherTooltipText.text = ""
            }
          }
        }
      }

      Timer {
        interval: 900000 // 15 minutes
        running: true
        repeat: true
        onTriggered: weatherProcess.exec(["bash", Quickshell.env("HOME") + "/git/gbastkowski/dotfiles/.config/waybar/scripts/weather-status.sh"])
        Component.onCompleted: weatherProcess.exec(["bash", Quickshell.env("HOME") + "/git/gbastkowski/dotfiles/.config/waybar/scripts/weather-status.sh"])
      }

      // Hover tooltip with the full weather details.
      PopupWindow {
        id: weatherTooltip
        visible: weatherMouse.containsMouse && weatherTooltipText.text !== ""
        anchor.window: bar
        anchor.rect.x: weatherLabel.mapToItem(null, 0, 0).x
        anchor.rect.y: bar.height + 4
        color: Qt.rgba(31 / 255, 31 / 255, 40 / 255, 0.95)

        Text {
          id: weatherTooltipText
          text: ""
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 14
          padding: 10
        }
      }

      // Right: system modules (network, bluetooth, volume, disk, load, temp, cpu, memory)
      Row {
        id: sysRow
        anchors {
          right: trayRow.left
          rightMargin: 14
          verticalCenter: parent.verticalCenter
        }
        spacing: 14
        visible: screenMonitor && screenMonitor.name === "DP-1"

        Text {
          id: netLabel
          text: "\uf0ac"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }

        Text {
          id: bluetoothLabel
          text: "󰂲"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20

          MouseArea {
            id: bluetoothMouse
            anchors.fill: parent
            hoverEnabled: true
            onClicked: Quickshell.execDetached(["bash", btScript, "--toggle"])
          }
        }

        Text {
          id: volumeLabel
          text: "\uf028"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20

          MouseArea {
            id: volumeMouse
            anchors.fill: parent
            hoverEnabled: true
            onClicked: Quickshell.execDetached(["wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])
          }

          WheelHandler {
            onWheel: function (event) {
              if (event.angleDelta.y > 0)
                Quickshell.execDetached(["wpctl", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", "5%+"])
              else
                Quickshell.execDetached(["wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%-"])
            }
          }
        }

        Text {
          id: diskLabel
          text: "󰋊 --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }

        Text {
          id: loadLabel
          text: "\uf080 --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }

        Text {
          id: tempLabel
          text: "\uf2c9 --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }

        Text {
          id: cpuLabel
          text: "\uf2db --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }

        Text {
          id: memLabel
          text: "󰍛 --"
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 20
        }
      }

      // System stats: network, disk, load, temperature, cpu, memory (3s poll).
      Process {
        id: sysProcess
        command: ["bash", sysScript]
        stdout: StdioCollector {
          onStreamFinished: {
            var d = null
            try { d = JSON.parse(text) } catch (e) {}
            if (!d) return
            if (d.netTooltip === "Disconnected") {
              netLabel.text = "\uf0ac"
            } else {
              netLabel.text = d.netWifi ? "󰤨" : "󰈀"
            }
            diskLabel.text = "󰋊 " + d.diskPct + "%"
            loadLabel.text = "\uf080 " + d.load
            tempLabel.text = "\uf2c9 " + d.temp + "°C"
            cpuLabel.text = "\uf2db " + d.cpu + "%"
            memLabel.text = "󰍛 " + d.memPct + "%"
          }
        }
      }

      Timer {
        interval: 3000
        running: true
        repeat: true
        onTriggered: sysProcess.exec(["bash", sysScript])
        Component.onCompleted: sysProcess.exec(["bash", sysScript])
      }

      // Bluetooth status (5s poll) + tooltip.
      Process {
        id: bluetoothProcess
        command: ["bash", btScript]
        stdout: StdioCollector {
          onStreamFinished: {
            var d = null
            try { d = JSON.parse(text) } catch (e) {}
            if (d && d.text) {
              bluetoothLabel.text = d.text
              bluetoothTipText.text = d.tooltip || ""
            }
          }
        }
      }

      Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: bluetoothProcess.exec(["bash", btScript])
        Component.onCompleted: bluetoothProcess.exec(["bash", btScript])
      }

      PopupWindow {
        id: bluetoothTooltip
        visible: bluetoothMouse.containsMouse && bluetoothTipText.text !== ""
        anchor.window: bar
        anchor.rect.x: bluetoothLabel.mapToItem(null, 0, 0).x
        anchor.rect.y: bar.height + 4
        color: Qt.rgba(31 / 255, 31 / 255, 40 / 255, 0.95)

        Text {
          id: bluetoothTipText
          text: ""
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 14
          padding: 10
        }
      }

      // Volume status (1s poll) + tooltip.
      Process {
        id: volumeProcess
        command: ["bash", volScript]
        stdout: StdioCollector {
          onStreamFinished: {
            var d = null
            try { d = JSON.parse(text) } catch (e) {}
            if (!d) return
            var label = d.text || "\uf028"
            var m = (d.tooltip || "").match(/Volume: (\d+)%/)
            if (m) label += " " + m[1] + "%"
            volumeLabel.text = label
            volumeTipText.text = d.tooltip || ""
          }
        }
      }

      Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: volumeProcess.exec(["bash", volScript])
        Component.onCompleted: volumeProcess.exec(["bash", volScript])
      }

      PopupWindow {
        id: volumeTooltip
        visible: volumeMouse.containsMouse && volumeTipText.text !== ""
        anchor.window: bar
        anchor.rect.x: volumeLabel.mapToItem(null, 0, 0).x
        anchor.rect.y: bar.height + 4
        color: Qt.rgba(31 / 255, 31 / 255, 40 / 255, 0.95)

        Text {
          id: volumeTipText
          text: ""
          color: "#f4e7c2"
          font.family: "Ubuntu Nerd Font"
          font.pixelSize: 14
          padding: 10
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
