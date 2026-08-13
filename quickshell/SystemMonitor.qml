import QtQuick
import QtQuick.Layouts
import Quickshell.Io

// System monitor: CPU / GPU / memory / network utilization with rolling
// sparkline graphs. Data comes from one long-running `sh` process that
// samples /proc/stat, /proc/meminfo, the amdgpu sysfs counters and
// /proc/net/dev every second.
Rectangle {
    id: root
    color: "transparent"

    property int cpu: 0
    property int gpu: 0
    property int memPct: 0
    property int memUsedKb: 0
    property int memTotalKb: 1
    // network throughput, bytes per second
    property int netDown: 0
    property int netUp: 0
    property real netMax: 1048576 // graph scale, auto-adjusted
    readonly property int histLen: 60

    property var cpuHist: []
    property var gpuHist: []
    property var memHist: []
    property var netDownHist: []
    property var netUpHist: []

    readonly property string memLabel:
        (root.memUsedKb / 1048576).toFixed(1) + "G"

    function push(hist, v) {
        hist.push(v)
        if (hist.length > root.histLen) hist.shift()
    }

    function fmtRate(b) {
        if (b >= 1048576) return (b / 1048576).toFixed(1) + "M"
        if (b >= 1024) return Math.round(b / 1024) + "K"
        return b + "B"
    }

    readonly property string monitorScript:
        'p_idle=0; p_total=0; p_rx=0; p_tx=0; n=0; '
      + 'while true; do '
      + 'read cpu user nice system idle iowait irq softirq steal rest < /proc/stat; '
      + 'idlet=$((idle + iowait)); total=$((user + nice + system + idle + iowait + irq + softirq + steal)); '
      + 'd_idle=$((idlet - p_idle)); d_total=$((total - p_total)); '
      + 'if [ "$d_total" -gt 0 ]; then cp=$((100 * (d_total - d_idle) / d_total)); else cp=0; fi; '
      + 'p_idle=$idlet; p_total=$total; '
      + 'while read -r k v rest; do k=${k%:}; [ "$k" = MemTotal ] && mt=$v; [ "$k" = MemAvailable ] && ma=$v; done < /proc/meminfo; mt=${mt:-1}; ma=${ma:-0}; '
      + 'mu=$((mt - ma)); mp=$((mu * 100 / mt)); '
      + 'gp=$(cat /sys/class/drm/card1/device/gpu_busy_percent 2>/dev/null || echo 0); '
      + 'rx_total=0; tx_total=0; '
      + 'while IFS= read -r l; do iface=${l%%:*}; case "$iface" in *lo*|*face*|*Inter*) continue;; esac; rest=${l#*: }; set -- $rest; [ -z "$1" ] && continue; rx_total=$((rx_total + $1)); tx_total=$((tx_total + $9)); done < /proc/net/dev; '
      + 'n=$((n + 1)); '
      + 'if [ "$n" -eq 1 ]; then drx=0; dtx=0; else drx=$((rx_total - p_rx)); dtx=$((tx_total - p_tx)); [ $drx -lt 0 ] && drx=0; [ $dtx -lt 0 ] && dtx=0; fi; '
      + 'p_rx=$rx_total; p_tx=$tx_total; '
      + 'echo "CPU $cp"; echo "MEM $mp $mu $mt"; echo "GPU $gp"; echo "NET $drx $dtx"; '
      + 'sleep 1; done'

    Process {
        id: monProc
        running: true
        command: ["sh", "-c", root.monitorScript]
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: function (line) {
                if (!line || line.length === 0) return
                var p = line.split(" ")
                if (p[0] === "CPU") {
                    root.cpu = parseInt(p[1]) || 0
                    root.push(root.cpuHist, root.cpu)
                    cpuCanvas.requestPaint()
                } else if (p[0] === "GPU") {
                    root.gpu = parseInt(p[1]) || 0
                    root.push(root.gpuHist, root.gpu)
                    gpuCanvas.requestPaint()
                } else if (p[0] === "MEM") {
                    root.memPct = parseInt(p[1]) || 0
                    root.memUsedKb = parseInt(p[2]) || 0
                    root.memTotalKb = parseInt(p[3]) || 1
                    root.push(root.memHist, root.memPct)
                    memCanvas.requestPaint()
                } else if (p[0] === "NET") {
                    root.netDown = parseInt(p[1]) || 0
                    root.netUp = parseInt(p[2]) || 0
                    root.push(root.netDownHist, root.netDown)
                    root.push(root.netUpHist, root.netUp)
                    // auto-scale the graph to the traffic seen (floor 1 MB/s)
                    var m = 1048576
                    for (var i = 0; i < root.netDownHist.length; i++) {
                        if (root.netDownHist[i] > m) m = root.netDownHist[i]
                        if (root.netUpHist[i] > m) m = root.netUpHist[i]
                    }
                    root.netMax = m * 1.3
                    netCanvas.requestPaint()
                }
            }
        }
    }

    component Sparkline: Canvas {
        required property var hist
        required property color lineColor
        required property color fillColor
        property var hist2: null // optional second series (line only)
        property color lineColor2: "#00000000"
        property real maxValue: 100
        readonly property int maxSamples: 60
        implicitHeight: 62

        function yFor(v) {
            var vv = Math.max(0, Math.min(v, maxValue))
            return height - 1 - (vv / maxValue) * (height - 2)
        }

        onPaint: {
            var ctx = getContext("2d")
            ctx.clearRect(0, 0, width, height)
            var h = hist
            if (h.length < 2 || maxValue <= 0) return
            var step = width / maxSamples
            var pts = []
            for (var i = 0; i < h.length; i++) {
                pts.push([width - (h.length - 1 - i) * step, yFor(h[i])])
            }
            // fill under the first series
            ctx.beginPath()
            ctx.moveTo(pts[0][0], height)
            for (i = 0; i < pts.length; i++) ctx.lineTo(pts[i][0], pts[i][1])
            ctx.lineTo(pts[pts.length - 1][0], height)
            ctx.closePath()
            ctx.fillStyle = fillColor
            ctx.fill()
            // first series line
            ctx.beginPath()
            ctx.moveTo(pts[0][0], pts[0][1])
            for (i = 1; i < pts.length; i++) ctx.lineTo(pts[i][0], pts[i][1])
            ctx.strokeStyle = lineColor
            ctx.lineWidth = 1.2
            ctx.stroke()
            // second series line (no fill)
            if (hist2 && hist2.length >= 2) {
                ctx.beginPath()
                ctx.moveTo(width - (hist2.length - 1) * step, yFor(hist2[0]))
                for (i = 1; i < hist2.length; i++) {
                    ctx.lineTo(width - (hist2.length - 1 - i) * step, yFor(hist2[i]))
                }
                ctx.strokeStyle = lineColor2
                ctx.lineWidth = 1.2
                ctx.stroke()
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.leftMargin: 12
        anchors.rightMargin: 12
        spacing: 10

        // CPU
        RowLayout {
            Layout.fillWidth: true
            spacing: 8
            Text {
                text: "CPU"
                color: "#8a8a8a"
                font.pixelSize: 14
                Layout.preferredWidth: 38
            }
            Sparkline {
                id: cpuCanvas
                hist: root.cpuHist
                lineColor: "#48bc00"
                fillColor: "#3048bc00"
                Layout.fillWidth: true
                Layout.preferredHeight: 62
            }
            Text {
                text: root.cpu + "%"
                color: "#e8e8e8"
                font.pixelSize: 15
                font.bold: true
                Layout.preferredWidth: 52
                horizontalAlignment: Text.AlignRight
            }
        }

        // GPU
        RowLayout {
            Layout.fillWidth: true
            spacing: 8
            Text {
                text: "GPU"
                color: "#8a8a8a"
                font.pixelSize: 14
                Layout.preferredWidth: 38
            }
            Sparkline {
                id: gpuCanvas
                hist: root.gpuHist
                lineColor: "#f0a030"
                fillColor: "#30f0a030"
                Layout.fillWidth: true
                Layout.preferredHeight: 62
            }
            Text {
                text: root.gpu + "%"
                color: "#e8e8e8"
                font.pixelSize: 15
                font.bold: true
                Layout.preferredWidth: 52
                horizontalAlignment: Text.AlignRight
            }
        }

        // Memory
        RowLayout {
            Layout.fillWidth: true
            spacing: 8
            Text {
                text: "MEM"
                color: "#8a8a8a"
                font.pixelSize: 14
                Layout.preferredWidth: 38
            }
            Sparkline {
                id: memCanvas
                hist: root.memHist
                lineColor: "#5ab0ff"
                fillColor: "#305ab0ff"
                Layout.fillWidth: true
                Layout.preferredHeight: 62
            }
            Text {
                text: root.memPct + "% " + root.memLabel
                color: "#e8e8e8"
                font.pixelSize: 15
                font.bold: true
                Layout.preferredWidth: 76
                horizontalAlignment: Text.AlignRight
            }
        }

        // Network (down = green, up = amber, auto-scaled)
        RowLayout {
            Layout.fillWidth: true
            spacing: 8
            Text {
                text: "NET"
                color: "#8a8a8a"
                font.pixelSize: 14
                Layout.preferredWidth: 38
            }
            Sparkline {
                id: netCanvas
                hist: root.netDownHist
                hist2: root.netUpHist
                lineColor: "#48bc00"
                fillColor: "#2048bc00"
                lineColor2: "#f0a030"
                maxValue: root.netMax
                Layout.fillWidth: true
                Layout.preferredHeight: 62
            }
            Text {
                text: "↓" + root.fmtRate(root.netDown) + " ↑" + root.fmtRate(root.netUp)
                color: "#e8e8e8"
                font.pixelSize: 13
                font.bold: true
                Layout.preferredWidth: 84
                horizontalAlignment: Text.AlignRight
            }
        }
    }
}
