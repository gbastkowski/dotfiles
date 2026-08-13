import QtQuick
import QtQuick.Layouts
import Quickshell.Io

// Berlin weather via Open-Meteo (no API key). Current conditions plus a
// 3-day min/max forecast. Refreshes every 30 minutes.
Rectangle {
    id: root
    color: "transparent"

    property string city: "Berlin"
    property int currentTemp: 0
    property string currentCondition: "–"
    property var days: []   // [{name, min, max, cond}]

    function wmoLabel(code) {
        if (code === 0) return "Clear"
        if (code <= 2) return "Partly cloudy"
        if (code === 3) return "Overcast"
        if (code === 45 || code === 48) return "Fog"
        if (code >= 51 && code <= 57) return "Drizzle"
        if (code >= 61 && code <= 67) return "Rain"
        if (code >= 71 && code <= 77) return "Snow"
        if (code >= 80 && code <= 82) return "Showers"
        if (code >= 85 && code <= 86) return "Snow showers"
        if (code >= 95) return "Thunderstorm"
        return "Unknown"
    }

    function refresh() {
        refreshProc.running = false
        refreshProc.running = true
    }

    readonly property string url:
        "https://api.open-meteo.com/v1/forecast"
      + "?latitude=52.52&longitude=13.41"
      + "&current=temperature_2m,weather_code"
      + "&daily=temperature_2m_max,temperature_2m_min,weather_code"
      + "&timezone=Europe%2FBerlin&forecast_days=3"

    Process {
        id: refreshProc
        running: false
        command: ["curl", "-s", "-m", "10", root.url]
        stdout: StdioCollector {
            onStreamFinished: root.parse(this.text)
        }
    }

    function parse(text) {
        try {
            var d = JSON.parse(text)
            root.currentTemp = Math.round(d.current.temperature_2m)
            root.currentCondition = root.wmoLabel(d.current.weather_code)
            var dd = d.daily
            var out = []
            var weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
            for (var i = 0; i < dd.time.length; i++) {
                var name = i === 0 ? "Today" : weekdays[new Date(dd.time[i] + "T12:00:00").getDay()]
                out.push({
                    name: name,
                    min: Math.round(dd.temperature_2m_min[i]),
                    max: Math.round(dd.temperature_2m_max[i]),
                    cond: root.wmoLabel(dd.weather_code[i])
                })
            }
            root.days = out
        } catch (e) {
            root.currentCondition = "offline"
        }
    }

    Timer {
        interval: 30 * 60 * 1000
        running: true
        repeat: true
        onTriggered: root.refresh()
    }

    Component.onCompleted: root.refresh()

    ColumnLayout {
        anchors.fill: parent
        spacing: 6

        // header + current
        RowLayout {
            Layout.fillWidth: true
            Text {
                text: root.city
                color: "#48bc00"
                font.pixelSize: 15
                font.bold: true
            }
            Item { Layout.fillWidth: true }
            Text {
                text: root.currentTemp + "°"
                color: "#e8e8e8"
                font.pixelSize: 28
                font.bold: true
            }
        }

        Text {
            Layout.fillWidth: true
            text: root.currentCondition
            color: "#9a9a9a"
            font.pixelSize: 12
        }

        // forecast rows
        Repeater {
            model: root.days
            delegate: RowLayout {
                Layout.fillWidth: true
                spacing: 8
                Text {
                    Layout.preferredWidth: 56
                    text: modelData.name
                    color: "#d8d8d8"
                    font.pixelSize: 13
                }
                Item { Layout.fillWidth: true }
                Text {
                    text: modelData.cond
                    color: "#9a9a9a"
                    font.pixelSize: 12
                    Layout.preferredWidth: 90
                }
                Text {
                    text: modelData.min + "° / " + modelData.max + "°"
                    color: "#e8e8e8"
                    font.pixelSize: 13
                    font.bold: true
                }
            }
        }
    }
}
