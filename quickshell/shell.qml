import Quickshell
import QtQuick
import QtQuick.Layouts
import "." as Local

// Left-edge sidebar hosting the AI chat panel.
// Reserves its width, so tiled windows are pushed out of the way.
ShellRoot {
    PanelWindow {
        anchors { left: true; top: true; bottom: true }
        implicitWidth: 320
        exclusiveZone: width
        focusable: true // needed so the chat input can receive keyboard input

        Rectangle {
            anchors.fill: parent
            color: "#e6111111" // translucent near-black

            Local.ChatPanel {
                anchors.fill: parent
            }
        }
    }
}
