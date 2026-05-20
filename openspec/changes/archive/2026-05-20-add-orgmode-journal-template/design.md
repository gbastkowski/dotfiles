## Context

`emacs/.doom.d/config.org` configures `org-journal` with a minimal file header lambda (lines 619–622) that emits only `#+TITLE` and `#+CATEGORY`.
The lambda is invoked at two sites:

1. `org-journal` itself when creating a new daily file.
2. `gunnar/org-copy-todo-to-today` (config.org:567) when refiling a task into a not-yet-existing journal file.

Both call paths receive the new template automatically — no additional wiring needed.

## Goals / Non-Goals

**Goals:**
- Extend the existing `org-journal-file-header` lambda so every new daily file starts with a hybrid GTD + Atomic Habits scaffold.
- Keep the change confined to a single source block in `config.org`.
- Leave existing journal files untouched.

**Non-Goals:**
- No new packages, keybindings, or agenda views.
- No custom commands for filling sections (e.g. evening review prompt).
- No org-roam-dailies integration.
- No habit tracker with `STYLE: habit` property + repeaters (deferred — add later if used).
- No retroactive template injection into existing files.

## Decisions

### Use a single multiline string inside the lambda

Concatenate template text inline (existing pattern) instead of introducing a helper function or `defvar`.

**Rationale:** lambda already builds the header by `concat`; single block keeps tangle simple. A helper would add indirection without reuse — `org-journal-file-header` is the only caller.

**Alternative considered:** define `gunnar/journal-template-string` as a `defvar` and reference it from the lambda. Rejected — premature abstraction for one call site.

### Identity/Energy as file-level keywords, not heading drawer

`:IDENTITY:` and `:ENERGY:` live as file-level `#+IDENTITY:` and `#+ENERGY:` keyword lines next to `#+TITLE` and `#+CATEGORY`.

**Rationale:** `org-journal-file-header` is invoked *before* org-journal inserts its day heading. Content emitted from the lambda lands above the first `*` heading, so a property drawer has no heading to attach to. File-level keywords are valid in that position and require no setting changes beyond the lambda.

**Alternative considered (rejected):** emit our own `* <date>` heading from the lambda and unset `org-journal-date-format`. Cleaner drawer attachment, but expands scope beyond a single lambda edit and changes the existing date heading produced by org-journal.

### Template sections sit above org-journal's day heading

Template `*`-level sections are emitted directly into the file header, so the resulting file looks like:

```
#+TITLE: 2026-05-19
#+CATEGORY: Journal
#+IDENTITY:
#+ENERGY: L/M/H

* Intention — the one thing
* Habits :habits:
...
* Evening review

* Journal               <-- org-journal inserts this on first entry
** 14:32                <-- normal org-journal time-stamped entry
```

**Rationale:** keeps the whole change inside the `org-journal-file-header` lambda; no extra hook, no settings change. The template's "Today's Next Actions" subsections become `** TODO :@work:` (one level above what the earlier draft used).

### Carryover behavior preserved

`org-journal-carryover-items "TODO=\"TODO|NEXT\""` continues to carry forward open TODOs from the previous day's file. Those land under the previous day's structure when org-journal copies them. Template sections (Intention, Habits, etc.) coexist with carried-over TODOs — no conflict.

## Risks / Trade-offs

- [Template grows stale / unused over time] → minimalist scope (single file, no commands). Cheap to revise the lambda later.
- [Carried-over TODOs may interleave awkwardly with template's "Next Actions" section] → Mitigation: observe behavior after first week; revisit `org-journal-carryover-items` placement in a follow-up change if it bites.
- [Template sits above the org-journal day heading, not under it] → accepted: keeps scope to a single lambda; identity/energy live as file-level keywords instead of a per-day drawer.

## Migration Plan

1. Edit the lambda in `config.org` (single org babel block).
2. Tangle (`C-c C-v t` or save hook) → `config.el` regenerates.
3. `doom sync` not required (no package change).
4. Reload config: `M-x doom/reload` or restart Emacs.
5. Create today's journal file (`M-x org-journal-new-entry`) to verify template renders.

Rollback: revert the single block in `config.org`, re-tangle.

## Open Questions

- None blocking. Habit-tracker integration (`STYLE: habit` + repeaters) deferred to a future change if daily checkboxes prove insufficient.
