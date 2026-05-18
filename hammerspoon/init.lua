-- Microsoft Teams mute toggle via local WebSocket API.
-- Hotkey: Cmd+Shift+Space.
-- Requires Teams: Settings -> Privacy -> Manage API -> third-party API enabled.

hs.allowAppleScript(true)
require("hs.ipc")

local TOKEN_FILE = os.getenv("HOME") .. "/.hammerspoon/teams_token"
local WS_URL_BASE = "ws://localhost:8124"
local MANUFACTURER = "Hammerspoon"
local DEVICE = "Mac"
local APP = "TeamsMuteHotkey"
local APP_VERSION = "1.0.0"

local ws = nil
local requestId = 0
local connected = false

local function readToken()
  local f = io.open(TOKEN_FILE, "r")
  if not f then return "" end
  local t = f:read("*a") or ""
  f:close()
  return (t:gsub("%s+$", ""))
end

local function writeToken(token)
  local f, err = io.open(TOKEN_FILE, "w")
  if not f then
    hs.alert.show("Teams: cannot save token: " .. tostring(err))
    return
  end
  f:write(token)
  f:close()
  os.execute("chmod 600 " .. TOKEN_FILE)
end

local function buildUrl()
  local token = readToken()
  return string.format(
    "%s?token=%s&protocol-version=2.0.0&manufacturer=%s&device=%s&app=%s&app-version=%s",
    WS_URL_BASE,
    hs.http.encodeForQuery(token),
    hs.http.encodeForQuery(MANUFACTURER),
    hs.http.encodeForQuery(DEVICE),
    hs.http.encodeForQuery(APP),
    hs.http.encodeForQuery(APP_VERSION)
  )
end

local function handleMessage(msg)
  local ok, data = pcall(hs.json.decode, msg)
  if not ok or type(data) ~= "table" then return end

  if data.tokenRefresh and data.tokenRefresh ~= "" then
    writeToken(data.tokenRefresh)
    hs.alert.show("Teams: paired, token saved")
  end

  if data.meetingUpdate then
    -- meetingUpdate.meetingState.isMuted available if needed
  end

  if data.errorMsg then
    hs.alert.show("Teams API: " .. tostring(data.errorMsg))
  end
end

local function connect()
  ws = hs.websocket.new(buildUrl(), function(status, msg)
    if status == "open" then
      connected = true
      print("Teams WS open")
    elseif status == "received" then
      handleMessage(msg)
    elseif status == "closed" or status == "fail" then
      connected = false
      print("Teams WS closed: " .. tostring(msg))
      hs.timer.doAfter(3, connect)
    end
  end)
end

local function sendAction(action)
  if not connected or not ws then
    hs.alert.show("Teams: not connected")
    return
  end
  requestId = requestId + 1
  local payload = hs.json.encode({
    action = action,
    parameters = {},
    requestId = requestId,
  })
  ws:send(payload)
end

connect()

hs.hotkey.bind({"cmd", "shift"}, "space", function()
  sendAction("toggle-mute")
end)

hs.alert.show("Teams mute hotkey loaded")
