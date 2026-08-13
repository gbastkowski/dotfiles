import QtQuick
import QtQuick.Layouts

// Simple current-month calendar (Monday-first, today highlighted).
Rectangle {
    id: root
    color: "transparent"

    property var cells: []
    property string monthLabel: ""

    readonly property int today: new Date().getDate()

    function build() {
        var now = new Date()
        var y = now.getFullYear(), m = now.getMonth()
        root.monthLabel = Qt.formatDate(now, "MMMM yyyy")
        var startDow = (new Date(y, m, 1).getDay() + 6) % 7 // Monday=0
        var days = new Date(y, m + 1, 0).getDate()
        var c = []
        for (var i = 0; i < startDow; i++) c.push("")
        for (var d = 1; d <= days; d++) c.push("" + d)
        root.cells = c
    }

    Component.onCompleted: root.build()

    ColumnLayout {
        anchors.fill: parent
        spacing: 6

        Text {
            Layout.alignment: Qt.AlignHCenter
            text: root.monthLabel
            color: "#48bc00"
            font.pixelSize: 15
            font.bold: true
        }

        // weekday header
        RowLayout {
            Layout.fillWidth: true
            spacing: 3
            Repeater {
                model: ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"]
                delegate: Text {
                    Layout.preferredWidth: root.width / 7
                    horizontalAlignment: Text.AlignHCenter
                    text: modelData
                    color: "#8a8a8a"
                    font.pixelSize: 12
                }
            }
        }

        // day grid
        Grid {
            id: grid
            Layout.fillWidth: true
            Layout.fillHeight: true
            columns: 7
            columnSpacing: 3
            rowSpacing: 3

            Repeater {
                model: root.cells
                delegate: Rectangle {
                    width: (grid.width - 6 * grid.columnSpacing) / 7
                    height: 26
                    radius: 5
                    color: modelData !== "" && parseInt(modelData) === root.today ? "#2f6a00" : "transparent"

                    Text {
                        anchors.centerIn: parent
                        text: modelData
                        color: modelData === ""
                            ? "transparent"
                            : (parseInt(modelData) === root.today ? "#ffffff" : "#d8d8d8")
                        font.pixelSize: 13
                        font.bold: parseInt(modelData) === root.today
                    }
                }
            }
        }
    }
}
