# Hyprland UX Design Learnings

Reference document for improving a Hyprland configuration with good UX in mind.

It distills design wisdom from the Hyprland community (wiki, r/hyprland, r/unixporn, tiling-WM discourse) and from the best-known dotfiles repositories: end-4/dots-hyprland, prasanthrangan/hyprdots, mylinuxforwork/dotfiles (ML4W), JaKooLit/Hyprland-Dots, cxOrz/dotfiles-hyprland, ahmad9059/HyprFlux and HyDE-Project/HyDE.

It doubles as the knowledge base for the mcp-hyprland config tutor: the principles below are what an agent checks a config against.

## Design principles

1. **Predictability first.**
   The user should always know where the next window will appear and what a keybind does.
   Deterministic layouts (dwindle), explicit layout switching, fixed workspace numbers.

2. **Muscle memory over memorization.**
   Fixed workspace numbers, one consistent modifier, stable layouts.
   "Think of it like a keyboard: you don't look for H, your finger knows where it is."

3. **Single-modifier dominance.**
   Use `SUPER` for everything.
   Modifier layers carry semantics: `SHIFT` = move/destructive, `CTRL` = layout/relative, `ALT` = silent/background.

4. **Home-row ergonomics.**
   High-frequency actions (close, terminal, launcher) live on home-row keys.
   Low-frequency actions (screenshots, power menu) get less accessible keys.

5. **Feedback for every state change.**
   Volume/brightness get an OSD, gamemode toggles notify, DND/special-workspace/mode states show in the bar.
   A state change without feedback is a silent failure.

6. **Animations serve workflow, not aesthetics.**
   Workspace switches < 200 ms; fade for open/close; launchers get `no_anim`.
   If an animation delays interaction, it is wrong.

7. **Discoverability is first-class.**
   `SUPER + /` keybind cheatsheet, searchable bind lists, submap overlays.
   If a bind needs a hint to be remembered, either document it or remove it.

8. **Workspaces are contexts, numbered and ephemeral.**
   Numbers 1-10 are universal and predictable.
   Workspaces are created on demand and destroyed when empty (`workspace_create_on_empty = true`).

9. **Scratchpad is the universal minimize.**
   A special workspace (`togglespecialworkspace`) stashes windows without losing them.

10. **Auto-float transient UI.**
    Dialogs, pickers, modals and PiP float automatically; main apps tile.
    The WM should know the difference.

11. **Multi-monitor needs workspace isolation.**
    Switching workspaces on monitor A must not disturb monitor B.
    Use workspace groups, per-monitor workspaces, or explicit `movewindowstomonitor` binds.

12. **Idle/lock is a staged pipeline.**
    Dim → lock → screen off → suspend, with gaps between stages and feedback at each.

13. **Reduce mouse dependence.**
    Every mouse interaction is a keyboard-interface failure.
    Focus, workspace switching and window management must be keyboard-reachable.

14. **Less is more.**
    20-30 core keybinds, a lean status bar, restrained animations.
    A lean setup is faster, more maintainable and easier to learn.

15. **Consistency across apps and states.**
    Media keys work everywhere (even on the lock screen, `bindl`).
    Volume/brightness/notifications use the same tools everywhere.

16. **Configuration is modular.**
    Defaults live separately from user overrides; updates never clobber customizations.
    Structure the config so a human (or agent) can reason about it.

## Keybind design

### Primary modifier

All major repos define the main modifier once and reuse it: `$mainMod = SUPER`.
`SUPER` (the Windows key) gives muscle-memory compatibility with Windows/GNOME.

Define it as a variable at the top, never hardcode:

```
$mainMod = SUPER
```

### Modifier layers

Consistent semantics across repos (hyprdots, JaKooLit, HyprFlux, HyDE):

| Layer | Meaning | Examples |
| --- | --- | --- |
| `SUPER` | core window management | Q close, arrows focus, J/K cycle |
| `SUPER + SHIFT` | move / destructive | `SHIFT+1..0` move window to workspace, `SHIFT+Q` kill process |
| `SUPER + CTRL` | layout / relative / silent | arrows move window, D remove master, `CTRL+Down` nearest empty workspace |
| `SUPER + ALT` | silent / background | `ALT+1..0` move to workspace silently, `ALT+S` move to scratchpad silently |
| `CTRL + ALT` | system-level | Delete exit, L lock, P power menu |

Silent variants (`movetoworkspacesilent`, `togglespecialworkspace` without focus change) are a signature of polished configs:
they let an agent or script rearrange windows without stealing the user's focus.

### Key codes over key names

Number-row binds use physical key codes so keyboard layouts (AZERTY, Dvorak) keep the same muscle memory:

```
bind = $mainMod, code:10, workspace, 1
bind = $mainMod, code:19, workspace, 10
```

Also bind numpad keys when workspaces exceed 10 (HyDE: `SUPER + KP_1..KP_0` = workspaces 11-20).

### Descriptions and metadata

Every bind gets a description; the format `[Category|Subcategory] action` powers hint systems and keeps the config self-documenting:

```
_F = {description = "[Window Management] close focused window"}
hl.bind(MOD .. " + Q", hl.dsp.window.close(), _F)
```

Flags that matter for UX (HyDE):

- `locked = true` — works on the lock screen (media keys, volume)
- `repeating = true` — hold to repeat (volume up/down)
- `release = true` — fire on key release (Alt-Tab switcher apply)
- `transparent = true` — passes the key through (Alt-Tab browse)

### Discoverability

Every good repo ships a built-in help system:

- `SUPER + /` — keybind cheatsheet (hyprdots, end-4, HyDE reads binds from the compositor itself and shows user overrides alongside defaults)
- Searchable bind list via launcher (JaKooLit `SUPER + SHIFT + K`)
- Submap overlays: entering a mode shows the relevant keys (discussion #10372)
- Hold-SUPER window-list overlay (evindor/window-list-overlay)

If the config has more binds than a cheatsheet can present, it has too many binds.

### Anti-patterns

- 100+ binds or 10+ submaps → unmaintainable and unmemorable
- Mixing `SUPER` and `ALT` for similar operations → breaks muscle memory
- Dynamic workspaces or auto-layout switching → disorienting
- Binds without `description` → the hint system is blind

## Workspace mental model

### Numbered, fixed

`SUPER + 1..0` maps to workspaces 1-10.
Numbers are universal: "go to workspace 3" beats "go to workspace code".
Assign workspaces to activities (1-2 code, 3 browser, 4 chat) so location is predictable.

### Ephemeral, on demand

`misc:workspace_create_on_empty = true` — switching to an empty workspace creates it.
`misc:close_special_on_empty = true` — scratchpads close themselves when empty.

`SUPER + CTRL + Down` → `workspace, empty` jumps to the nearest empty workspace (hyprdots, HyDE).

### Scratchpad

```
bind = $mainMod, S, togglespecialworkspace, nyx
bind = $mainMod SHIFT, S, movetoworkspace, special:nyx
bind = $mainMod ALT, S, movetoworkspace, special:nyx   # silent variant
```

Dedicate one special workspace to transient tools (drop terminal, notes).
end-4 gives the scratchpad larger gaps (`gaps_out = 30`) to distinguish it visually.

### Per-monitor isolation

- end-4: workspace groups (1-10, 11-20, 21-30); each monitor focuses its own group
- JaKooLit: `SUPER + CTRL + F9-F12` moves the workspace to another monitor
- Hyprsplit plugin (persistent per-monitor workspaces) is a legitimate alternative to the shared 1-10 scheme

### Empty-workspace rules

`workspace = 2, layout_opts: { orientation = "top" }` — per-workspace layout rules reinforce the workspace's context identity.
`on-created-empty` executes setup when a workspace is created (e.g. launch the assigned app).

## Window management UX

### Auto-float transient UI

Float dialogs, pickers and modals; tile everything else:

- class/title regexes: `Open File`, `Save As`, `Picture-in-Picture`, `pavucontrol`, `.*dialog.*`
- HyDE compiles the regexes once and reuses them (`util.regex_compile`) for performance
- HyprFlux organizes rules by tag (`browser`, `notif`, `terminal`, `im`) for consistent application
- HyDE: `modal = true` → float, center, pin (modal dialogs)
- HyDE: PiP → float, pin, move to `monitor_w*0.73, monitor_h*0.72`, size `25%x25%`

Center floating dialogs (`center = on`).

### Gaps

`general:gaps_in` / `general:gaps_out` give breathing room and make window boundaries obvious.
Game mode sets them to 0.
Larger gaps on the scratchpad distinguish it from regular workspaces.

### Animation policy

- Workspace switches < 200 ms (300 ms+ interrupts flow)
- `fade` for window open/close — fast, non-disorienting
- Name and reuse bezier curves (HyprFlux: `wind`, `winIn`, `winOut`, `overshot`) for a consistent motion language
- Launchers and quick panels get `no_anim = true` — instant feedback matters most there
- HyDE ships selectable animation presets (gnome, macos, end4, fast, minimal); spring-based curves scale duration
- Gaming mode (below) is the escape hatch

### Focus behavior

- `input:follow_mouse = 1` — focus follows mouse; the community default
- `input:focus_on_close = next` — predictable focus after close
- `dwindle:force_split = 2` — always split right so new windows get keyboard focus predictably
- `no_initial_focus` window rule — for notification daemons, screen-share bridges (xwaylandvideobridge), background helpers
- `misc:focus_on_activate = false` by default; enable per-window (browser yes, popups no)
- `stay_focused` for pinentry/dialogs; `idle_inhibit` to protect video playback from the lock

### Gaming mode

One keybind toggles a performance profile (JaKooLit `SUPER + SHIFT + G`, ML4W `SUPER + ALT + G`, HyDE `SUPER + ALT + G`):

```
hyprctl --batch "\
    keyword animations:enabled 0;\
    keyword decoration:drop_shadow 0;\
    keyword decoration:blur:enabled 0;\
    keyword general:gaps_in 0;\
    keyword general:gaps_out 0"
```

Always show feedback when toggling (notification: "Gamemode enabled").
It must be a toggle, never a config edit.

## Feedback and discoverability

### Notifications

- Urgency classes: low (5 s), normal (10 s), critical (no timeout, stays until dismissed)
- Group by app; replace-in-place via `string:x-canonical-private-synchronous:<id>` (no notification spam for repeated volume changes)
- DND silences but still logs; critical apps bypass via override-urgency
- Provide action buttons where useful; `hide-on-action`

### OSD for volume/brightness

Use a dedicated overlay (swayosd, avizo, wob), not the status bar.
An OSD is transient, centered, shows icon + progress, auto-hides after ~2 s, and never steals focus.
Separate OSDs for volume and brightness, same visual style.

### Status bar hierarchy

- Left: navigation (workspace indicators always visible, active window, mode indicator)
- Center: clock / mode (the focus anchor)
- Right: system health (battery, network, audio, tray; CPU/RAM hover-only)
- Frequency of use determines visibility — constantly-checked info always visible, occasional info on hover
- Icons over text; one accent color for active states

### Launcher UX

- Show what the launcher can do: rofi mode switcher row, walker prefix list (`;`)
- Fuzzy search is forgiving (fzf-style), accommodates typos
- Big icons when few matches (fuzzel) = visual confirmation
- Menus as a unified ecosystem (HyDE): apps, windows, files, emoji, clipboard, calculator, web search — all via `hyde.sh.menu.*`

### Overview / task switching

- Thumbnail overviews beat text lists (end-4 overview, hyprexpo, HyDE window menu)
- Alt-Tab that behaves like a browser: hold `ALT` to browse, release to switch (HyDE altab)
- Hold-SUPER window list overlay for scrolling layouts

### Every state needs an indicator

- DND active → bar icon
- Special workspace open → distinct highlight in workspace indicator
- Binding mode active → mode name in bar or overlay
- Idle inhibitor active → visible somewhere

## Idle and lock flow

### Staged pipeline

```
general {
    lock_cmd = pidof hyprlock || hyprlock
    before_sleep_cmd = loginctl lock-session
    after_sleep_cmd = hyprctl dispatch dpms on
}

listener { timeout = 150; on-timeout = brightnessctl -s set 10; on-resume = brightnessctl -r }
listener { timeout = 300; on-timeout = loginctl lock-session }
listener { timeout = 330; on-timeout = hyprctl dispatch dpms off; on-resume = hyprctl dispatch dpms on }
listener { timeout = 1800; on-timeout = systemctl suspend }
```

- Stagger the timeouts (30-60 s gaps) so the user can react at each stage
- Each stage gives feedback: dim (warning) → lock (password) → off (black) → suspend (LED)
- HyprFlux adds an explicit idle warning notification before locking (`notify-send "You are idle!"`)
- `lock_cmd = pidof hyprlock || hyprlock` avoids multiple lock instances
- `after_sleep_cmd` turns the display back on, otherwise waking needs a double keypress
- `condition_cmd` gates actions (e.g. do not suspend while an SSH session runs), `condition_retry` re-checks
- `idle_inhibit` window rule: video players and browsers hold the lock open; `ignore_wayland_inhibit` opts a listener out
- Let systemd-logind handle the laptop lid; let hypridle handle idle

## Modularity and config organization

- Defaults in `configs/`, user overrides in `UserConfigs/` sourced at the end (HyprFlux, JaKooLit)
- HyDE: `~/.local/share/hypr/lua/` (system, overwritten on update) vs `~/.config/hypr/hyprland.lua` (user, preserved, loaded last so settings take precedence)
- Lua config (HyDE) enables programmatic generation, bind deduplication (`hyde.binds.dedup`), and structured metadata
- A first-run script (HyprFlux `initial-boot.sh` + `.initial_startup_done` marker) handles onboarding
- Theme engines (HyDE) keep visuals consistent across the whole desktop from one source

## Common UX failures

- **Over-animation**: pretty but slow; workspace switches over 300 ms break flow
- **Too many keybinds**: if you need a searchable menu, you have too many
- **Inconsistent modifiers**: mixing SUPER and ALT for similar actions
- **Hidden states**: DND, special workspace, mode, idle inhibit without any indicator
- **Breaking muscle memory**: dynamic workspaces, auto-layout switching, non-SUPER primary mod
- **Workspace overload**: 20+ workspaces nobody can remember; switch blindly until found
- **Bar as OSD**: waybar is a status bar, not a feedback overlay

## Audit checklist

Check a config against these; each item maps to a principle above.

- [ ] Main modifier defined once as a variable (`$mainMod = SUPER`)
- [ ] Modifier layers are consistent (SHIFT = move, CTRL = layout, ALT = silent)
- [ ] Number-row binds use key codes (`code:10`..`code:19`)
- [ ] Every bind has a description; `SUPER + /` shows a cheatsheet
- [ ] Binds stay in the 20-30 core range; rare actions live in submaps or the launcher
- [ ] Workspaces are numbered and <= 10 (or a deliberate scheme like hyprsplit)
- [ ] `workspace_create_on_empty` enabled; empty-workspace jump bound
- [ ] Scratchpad bound: toggle, move, and a silent move variant
- [ ] Silent workspace movement exists (`movetoworkspacesilent`)
- [ ] Dialogs/pickers/modals auto-float and center; PiP is pinned
- [ ] Notification daemon gets `no_initial_focus`; screen-share bridges too
- [ ] Animations: workspace switches < 200 ms, fade open/close, launchers `no_anim`
- [ ] Game mode toggle bound, with feedback
- [ ] Volume/brightness change shows an OSD; media keys work on the lock screen (`bindl`, `locked`)
- [ ] Every state toggle (DND, special, mode, gamemode) has a visible indicator
- [ ] Idle chain is staged: dim → lock → off → suspend, with gaps and feedback
- [ ] Defaults separated from user overrides; config is structured and commented

## Sources

- end-4/dots-hyprland (Quickshell shell; workspace groups; scratchpad gaps; SUPER+/ cheatsheet)
- prasanthrangan/hyprdots (modifier layers; silent variants; empty-workspace jump; gamemode batch; cursor-aware menus)
- mylinuxforwork/dotfiles ML4W (game mode; keybindings docs; dunst/waybar conventions)
- JaKooLit/Hyprland-Dots (key codes; drop terminal; cliphist; media keys; notification patterns)
- ahmad9059/HyprFlux (tag-based window rules; staged idle warning; UserConfigs override pattern)
- HyDE-Project/HyDE (Lua config; description metadata; bind flags; regex-compiled rules; alt-tab; animation presets; theme engine)
- Hyprland wiki (focus/cursor/workspace options; hypridle; window rules)
- r/hyprland, r/unixporn, i3 discussions #4700, Hyprland discussions #12357, #1793, #10372

