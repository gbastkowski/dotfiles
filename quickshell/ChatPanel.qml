import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland

// AI chat panel for the sidebar. Two interchangeable backends, toggled in
// the header:
//   - opencode:  local `opencode serve` (REST API + SSE /event stream),
//                routed to opencode-go/deepseek-v4-flash via a dedicated
//                minimal config. The server is auto-started detached if
//                it isn't running.
//   - ollama:    streaming NDJSON from the local Ollama daemon.
Rectangle {
    id: root

    // Backend: opencode serve, routed to the local ollama provider
    // (qwen3.5:9b via the ai/opencode.json config). Direct-ollama code stays
    // below as a fallback: set this to false to use it.
    property bool useOpencode: true
    property string ollamaModel: "qwen3.5:9b" // answers without narrating its thinking
    property string ocModel: "qwen3.5:9b" // served by the ollama provider
    property string ocEndpoint: "http://127.0.0.1:4097"
    property string ocPort: "4097"
    property string ocAgent: "chat"

    // Resolved next to this QML file: ai/opencode.json in the config dir.
    readonly property string ocConfigPath: Qt.resolvedUrl("ai/opencode.json").toString().replace(/^file:\/\//, "")
    readonly property string ocWorkDir: {
        var i = root.ocConfigPath.lastIndexOf("/")
        return root.ocConfigPath.substring(0, i)
    }

    // Backend status: ollama -> "" | "no-daemon" | "no-model" | "ok"
    //                 opencode -> "off" | "starting" | "on" | "error"
    property string ollamaStatus: ""
    property string ocStatus: "off"
    property bool busy: false
    readonly property bool ready: useOpencode ? ocStatus === "on" : ollamaStatus === "ok"

    // Conversation history: [{role: "user"|"assistant", content: string}]
    property var history: []
    property string streamingText: ""
    // Epoch guard: bumped on send/cancel/clear so late chunks from an
    // aborted request can never backwrite the list.
    property int _epoch: 0
    property int _activeEpoch: -1
    property string _pendingPrompt: ""
    property string _pendingAgent: ""

    // opencode session (one per conversation) and last raw response
    property string ocSessionID: ""
    property string lastOcResponse: ""

    color: "transparent"

    // grabs keyboard focus for the panel window (used by the global shortcut)
    HyprlandFocusGrab {
        id: focusGrab
    }

    ListModel { id: chatModel }

    // ---- shared UI state -------------------------------------------------
    readonly property string statusText: {
        if (useOpencode) {
            if (ocStatus === "starting") return "starting opencode…"
            if (ocStatus === "error") return "⚠ opencode server failed to start"
            return ""
        }
        if (ollamaStatus === "no-daemon") return "⚠ ollama not running (ollama serve)"
        if (ollamaStatus === "no-model") return "⚠ model " + root.ollamaModel + " not pulled"
        if (ollamaStatus !== "ok") return "checking ollama…"
        return ""
    }

    // ---- ollama backend --------------------------------------------------
    Process {
        id: probeProc
        running: false
        command: ["sh", "-c",
            "if ! curl -s --max-time 1 http://localhost:11434/api/tags >/dev/null 2>&1; then echo no-daemon; exit; fi; "
            + "if ! curl -s http://localhost:11434/api/tags | grep -Fq -- \"$1\"; then echo no-model; exit; fi; "
            + "echo ok",
            "sh", root.ollamaModel]
        stdout: StdioCollector {
            onStreamFinished: { root.ollamaStatus = this.text.trim(); }
        }
    }

    Process {
        id: chatProc
        running: false
        command: ["true"]
        property int gen: 0
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: function (data) {
                if (chatProc.gen !== root._epoch) return // stale stream
                if (!root.busy || !data || data.length === 0) return
                var obj = null
                try { obj = JSON.parse(data) } catch (e) { return }
                if (obj.done) { root.finishTurn(); return }
                var chunk = obj.message && obj.message.content
                if (typeof chunk === "string" && chunk.length > 0) {
                    root.streamingText += chunk
                    root.updateStreamingRow()
                }
            }
        }
        onExited: {
            // abort (SIGTERM) or crash: keep whatever streamed
            if (root.busy && !root.useOpencode) root.finishTurn()
        }
    }

    // ---- opencode backend ------------------------------------------------
    // health probe
    Process {
        id: ocHealthProc
        running: false
        command: ["true"]
        stdout: StdioCollector {
            onStreamFinished: { root.onOcHealth(this.text.trim()) }
        }
    }

    // detached server process: command/env/workdir set right before startDetached()
    Process {
        id: ocStartProc
        running: false
        command: ["true"]
    }

    // session creation (response carries the session id)
    Process {
        id: ocSessProc
        running: false
        command: ["true"]
        property string pendingPrompt: ""
        stdout: StdioCollector {
            onStreamFinished: { root.onOcSessionCreated(this.text.trim()) }
        }
    }

    // message POST (blocking; response is the final message, authoritative)
    Process {
        id: ocMsgProc
        running: false
        command: ["true"]
        stdout: StdioCollector {
            onStreamFinished: { root.lastOcResponse = this.text; }
        }
        onExited: { if (root.busy && root.useOpencode) root.finishOcTurn() }
    }

    // persistent SSE stream of server events (token deltas)
    Process {
        id: ocEventProc
        running: false
        command: ["curl", "-sN", root.ocEndpoint + "/event"]
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: function (data) { root.handleOcEvent(data) }
        }
        onExited: {
            // server died or stream dropped: reconnect if still wanted
            if (root.useOpencode && root.ocStatus === "on") {
                reconnectTimer.start()
            }
        }
    }

    Timer {
        id: reconnectTimer
        interval: 1500
        onTriggered: {
            if (root.useOpencode && root.ocStatus === "on" && !ocEventProc.running) {
                ocEventProc.running = false
                ocEventProc.running = true
            }
        }
    }

    Timer {
        id: ocHealthTimer
        interval: 600
        repeat: true
        onTriggered: root.ocProbe()
    }

    property int _ocStartAttempted: 0

    function ocProbe() {
        ocHealthProc.command = ["curl", "-s", "-m", "2", root.ocEndpoint + "/global/health"]
        ocHealthProc.running = false
        ocHealthProc.running = true
    }

    function onOcHealth(text) {
        if (text.indexOf('"healthy":true') >= 0) {
            root.ocStatus = "on"
            ocHealthTimer.stop()
            if (!ocEventProc.running) {
                ocEventProc.running = false
                ocEventProc.running = true
            }
            if (root._pendingPrompt) {
                var p = root._pendingPrompt
                var ag = root._pendingAgent || root.ocAgent
                root._pendingPrompt = ""
                root._pendingAgent = ""
                root.appendTurn(p, ag === "rangun" ? "rangun" : "chat")
                if (root.ocSessionID.length === 0) {
                    ocSessProc.pendingPrompt = p
                    ocSessProc.command = ["curl", "-s", "-m", "10",
                        "-X", "POST", root.ocEndpoint + "/session",
                        "-H", "Content-Type: application/json",
                        "-d", JSON.stringify({ agent: ag, title: "sidebar" })]
                    ocSessProc.running = false
                    ocSessProc.running = true
                } else {
                    root.postOcMessage(p, ag)
                }
            }
        } else if (root.ocStatus !== "off" && root._ocStartAttempted < 2) {
            // not up yet: on first probe attempts, launch the server detached
            if (root._ocStartAttempted === 0) {
                root._ocStartAttempted += 1
                ocStartProc.command = ["opencode", "serve", "--port", root.ocPort, "--hostname", "127.0.0.1"]
                // isolate from ~/.config/opencode: the widget config self-contains
                // providers + plugins, so the server must not merge global configs.
                ocStartProc.environment = { XDG_CONFIG_HOME: root.ocWorkDir + "/xdg" }
                ocStartProc.workingDirectory = root.ocWorkDir
                ocStartProc.startDetached() // instance method, no args
            } else {
                root._ocStartAttempted += 1
            }
        } else if (root._ocStartAttempted >= 2 && root.ocStatus !== "off") {
            root.ocStatus = "error"
            ocHealthTimer.stop()
        }
    }

    function ocEnsure() {
        if (root.ocStatus === "on" || root.ocStatus === "starting") return
        root.ocStatus = "starting" // also retries after "error"
        root._ocStartAttempted = 0
        ocHealthTimer.start()
    }

    function handleOcEvent(line) {
        if (!root.busy || root._activeEpoch !== root._epoch) return
        if (!line || line.indexOf("data:") !== 0) return
        var obj = null
        try { obj = JSON.parse(line.substring(5).trim()) } catch (e) { return }
        if (!obj || obj.type !== "message.part.delta") return
        var props = obj.properties || {}
        if (props.sessionID !== root.ocSessionID) return
        if (props.field === "text" && typeof props.delta === "string") {
            root.streamingText += props.delta
            root.updateStreamingRow()
        }
    }

    function onOcSessionCreated(text) {
        var obj = null
        try { obj = JSON.parse(text) } catch (e) {}
        if (!obj || !obj.id) {
            // create failed: surface as a plain failed turn
            root.busy = true
            root.finishTurnWith("⚠ opencode session error")
            return
        }
        root.ocSessionID = obj.id
        root.postOcMessage(ocSessProc.pendingPrompt)
    }

    function postOcMessage(prompt, agent) {
        root.streamingText = ""
        root.busy = true
        root._activeEpoch = root._epoch
        root.lastOcResponse = ""
        ocMsgProc.command = ["curl", "-sN", "-m", "300",
            "-X", "POST", root.ocEndpoint + "/session/" + root.ocSessionID + "/message",
            "-H", "Content-Type: application/json",
            "-d", JSON.stringify({ agent: agent || root.ocAgent, parts: [{ type: "text", text: prompt }] })]
        ocMsgProc.running = false
        ocMsgProc.running = true
        root.updateStreamingRow()
    }

    function sendOc(prompt, agent) {
        if (root.ocStatus !== "on") {
            root._pendingPrompt = prompt // flushed by onOcHealth once the server is up
            root._pendingAgent = agent || root.ocAgent
            root.ocEnsure()
            return
        }
        root.appendTurn(prompt, agent === "rangun" ? "rangun" : "chat")
        root._pendingPrompt = ""
        var ag = agent || root.ocAgent
        if (root.ocSessionID.length === 0) {
            ocSessProc.pendingPrompt = prompt
            ocSessProc.command = ["curl", "-s", "-m", "10",
                "-X", "POST", root.ocEndpoint + "/session",
                "-H", "Content-Type: application/json",
                "-d", JSON.stringify({ agent: ag, title: "sidebar" })]
            ocSessProc.running = false
            ocSessProc.running = true
        } else {
            root.postOcMessage(prompt, ag)
        }
    }

    function finishTurn() { // ollama path
        if (!root.busy) return
        root.busy = false
        root.history.push({ role: "assistant", content: root.streamingText })
    }

    function finishTurnWith(text) { // opencode fallback (error/empty)
        root.busy = false
        root.streamingText = text
        root.setAssistantRow(text)
        root.history.push({ role: "assistant", content: text })
        chatList.positionViewAtEnd()
    }

    function finishOcTurn() {
        if (!root.busy) return
        root.busy = false
        // authoritative text from the POST response beats streamed deltas
        var finalText = root.streamingText
        if (root.lastOcResponse) {
            try {
                var d = JSON.parse(root.lastOcResponse)
                var parts = d.parts || []
                var txt = ""
                for (var i = 0; i < parts.length; i++) {
                    if (parts[i].type === "text") txt += parts[i].text
                }
                if (txt.length > 0) finalText = txt
            } catch (e) {}
        }
        if (finalText.length === 0) finalText = "⚠ no response from opencode"
        root.streamingText = finalText
        root.setAssistantRow(finalText)
        root.history.push({ role: "assistant", content: finalText })
        chatList.positionViewAtEnd()
    }

    // ---- shared logic ----------------------------------------------------
    // update the streaming row, preserving its source tag (chat/rangun)
    function setAssistantRow(text) {
        var src = "chat"
        if (chatModel.count > 0) src = chatModel.get(chatModel.count - 1).source || "chat"
        chatModel.set(chatModel.count - 1, { role: "assistant", text: text, source: src })
    }

    function updateStreamingRow() {
        root.setAssistantRow(root.streamingText)
        chatList.positionViewAtEnd()
    }

    function appendTurn(prompt, source) {
        var src = source || "chat"
        chatModel.append({ role: "user", text: prompt, source: src })
        root.history.push({ role: "user", content: prompt })
        chatModel.append({ role: "assistant", text: "", source: src })
        root._epoch += 1
        chatProc.gen = root._epoch
        chatList.positionViewAtEnd()
    }

    function send() {
        if (root.busy) return
        var prompt = input.text.trim()
        if (prompt.length === 0) return
        input.text = ""
        // /rangun <question> delegates to the Rangun assistant (Hermes on smarty:3000)
        if (prompt === "/rangun" || prompt.indexOf("/rangun ") === 0) {
            var rq = prompt.length > 7 ? prompt.substring(8).trim() : ""
            if (rq.length > 0) { root.sendOc(rq, "rangun"); return }
            return
        }
        if (root.useOpencode) { root.sendOc(prompt); return }
        // ollama
        root.appendTurn(prompt)
        root.streamingText = ""
        root.busy = true
        root._activeEpoch = root._epoch
        var payload = {
            model: root.ollamaModel,
            messages: root.history.concat([{ role: "system", content: root.systemPrompt }]),
            stream: true,
            think: false
        }
        chatProc.command = ["curl", "-sN",
            "http://localhost:11434/api/chat",
            "-d", JSON.stringify(payload)]
        chatProc.running = false
        chatProc.running = true
        chatList.positionViewAtEnd()
    }

    function cancel() {
        root._epoch += 1 // invalidate in-flight chunks
        if (root.useOpencode) {
            ocMsgProc.running = false // SIGTERM the POST; deltas stop matching epoch
        } else {
            chatProc.running = false // SIGTERM; onExited finalizes the partial
        }
    }

    function clearChat() {
        root._epoch += 1
        root.busy = false
        root.streamingText = ""
        root.history = []
        chatModel.clear()
        if (root.useOpencode) {
            root.ocSessionID = "" // fresh session on next send
            ocMsgProc.running = false
            ocSessProc.running = false
            root.lastOcResponse = ""
        }
        input.forceActiveFocus()
    }

    // Focus the chat input and give the panel keyboard focus (global shortcut).
    function focusInput() {
        var win = QsWindow.window
        if (win) {
            focusGrab.windows = [win]
            focusGrab.active = true
        }
        input.forceActiveFocus()
    }

    // Programmatic entry point (used by tests / external bindings).
    function submitPrompt(text) {
        if (!root.ready || root.busy) return
        input.text = text
        root.send()
    }

    readonly property string systemPrompt:
        "You are a terse assistant for a Linux / Hyprland user. "
      + "Reply concisely, no preamble, no restating the question. "
      + "Wrap shell commands or code in fenced ```code``` blocks. "
      + "If you don't know, say so in one line."

    Component.onCompleted: {
        if (root.useOpencode) root.ocEnsure()
        else { probeProc.running = false; probeProc.running = true }
    }

    // ---- UI --------------------------------------------------------------
    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        spacing: 8

        // header
        Row {
            Layout.fillWidth: true
            Layout.preferredHeight: 26
            spacing: 6
            Text {
                height: 22
                text: "AI Chat"
                color: "#48bc00"
                font.pixelSize: 17
                font.bold: true
                verticalAlignment: Text.AlignVCenter
            }
            Item { Layout.fillWidth: true; height: 1 }
            Text {
                height: 22
                text: "clear"
                color: root.busy ? "#555" : "#8a8a8a"
                font.pixelSize: 13
                verticalAlignment: Text.AlignVCenter
                MouseArea {
                    anchors.fill: parent
                    enabled: !root.busy
                    onClicked: root.clearChat()
                }
            }
        }

        // status line
        Text {
            Layout.fillWidth: true
            visible: root.statusText.length > 0
            text: root.statusText
            color: "#ff6b6b"
            font.pixelSize: 13
            wrapMode: Text.Wrap
        }

        // messages
        ListView {
            id: chatList
            Layout.fillWidth: true
            Layout.fillHeight: true
            model: chatModel
            clip: true
            spacing: 8

            delegate: Item {
                id: wrap
                required property string role
                required property string text
                property string source: "chat"
                readonly property bool isUser: role === "user"
                readonly property bool isRangun: source === "rangun"
                width: chatList.width
                height: bubble.height

                Rectangle {
                    id: bubble
                    property bool isUser: wrap.isUser
                    property bool isRangun: wrap.isRangun
                    x: isUser ? wrap.width - width : 0
                    width: Math.min(msg.implicitWidth + 24, wrap.width - 24)
                    height: msg.implicitHeight + 24
                    radius: 10
                    color: isRangun ? (isUser ? "#0e2a4a" : "#14222e") : (isUser ? "#1e4a00" : "#2a2a2a")
                    border.width: isRangun ? 2 : 1
                    border.color: isRangun ? "#3f6fb0" : (isUser ? "#2f6a00" : "#3a3a3a")

                    Text {
                        id: msg
                        x: 12
                        y: 12
                        width: bubble.width - 24
                        text: wrap.text
                        color: "#e8e8e8"
                        font.pixelSize: 16
                        wrapMode: Text.Wrap
                    }
                }
            }
        }

        // input row
        Row {
            Layout.fillWidth: true
            Layout.preferredHeight: 50
            spacing: 8

            Rectangle {
                id: inputBox
                width: parent.width - sendBtn.width - parent.spacing
                height: 50
                radius: 8
                color: "#1d1d1d"
                border.width: 1
                border.color: root.busy ? "#48bc00" : "#333"

                Text {
                    id: placeholder
                    anchors.fill: input
                    anchors.leftMargin: 10
                    visible: input.text.length === 0
                    text: root.busy ? "generating…"
                        : !root.ready ? "starting…"
                        : "ask…"
                    color: "#5a5a5a"
                    font.pixelSize: 16
                    verticalAlignment: Text.AlignVCenter
                    z: -1
                    enabled: false
                }

                TextInput {
                    id: input
                    anchors.fill: parent
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    color: "#eee"
                    font.pixelSize: 16
                    verticalAlignment: Text.AlignVCenter
                    clip: true
                    enabled: root.ready
                    Keys.onReturnPressed: root.send()
                    Keys.onEnterPressed: root.send()
                }
            }

            Rectangle {
                id: sendBtn
                width: 52
                height: 50
                radius: 8
                color: root.busy ? "#7a1f1f" : (root.ready ? "#48bc00" : "#333")

                Text {
                    anchors.centerIn: parent
                    text: root.busy ? "■" : "➤"
                    color: "white"
                    font.pixelSize: 18
                }

                MouseArea {
                    anchors.fill: parent
                    onClicked: root.busy ? root.cancel() : root.send()
                }
            }
        }
    }

    // focus the input when the panel is clicked
    MouseArea {
        anchors.fill: parent
        z: -1
        onClicked: input.forceActiveFocus()
    }
}
