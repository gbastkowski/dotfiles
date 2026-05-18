-- Global hotkey: Cmd+Shift+Space toggles Microsoft Teams mute.
-- Sends Cmd+Shift+M to Teams without stealing focus.

hs.hotkey.bind({"cmd", "shift"}, "space", function()
  local teams = hs.application.find("Microsoft Teams")
  if not teams then
    hs.alert.show("Teams not running")
    return
  end
  hs.osascript.applescript([[
    tell application "System Events"
      tell process "Microsoft Teams"
        key code 46 using {command down, shift down}
      end tell
    end tell
  ]])
end)

hs.alert.show("Hammerspoon config loaded")
