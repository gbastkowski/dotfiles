## Why

Daily journal files created by `org-journal` currently start with only `#+TITLE` and `#+CATEGORY` — no structure to guide a daily practice.
Adding a hybrid GTD + Atomic Habits template at file creation time turns each journal file into a ready-to-fill scaffold (intention, habits, next actions, inbox, evening review) without manual setup or extra packages.

## What Changes

- Extend `org-journal-file-header` lambda in `emacs/.doom.d/config.org` to emit a structured hybrid template after the existing `#+TITLE` / `#+CATEGORY` lines.
- Template sections (file-level keywords + level-1 org headings):
  - `#+IDENTITY:` and `#+ENERGY: L/M/H` file-level keywords (next to `#+TITLE` / `#+CATEGORY`)
  - `* Intention — the one thing`
  - `* Habits :habits:` (3 concrete checkboxes: Use pomodoro / Eat apple / Do exercise)
  - `* Evening review` (checkboxes + Wins / Friction / Tomorrow's one thing)
- No package additions. No keybindings. No agenda view changes.

## Capabilities

### New Capabilities
- `journal-template`: defines the structure and content emitted at daily journal file creation time.

### Modified Capabilities
<!-- none -->

## Impact

- File: `emacs/.doom.d/config.org` (single block around line 619).
- Tangled output: `emacs/.doom.d/config.el` regenerates via Doom literate config tangle on save — out of scope to edit by hand.
- Affected behavior: only *new* journal files; existing files unchanged.
- No dependency, package, or workflow changes. `gunnar/org-copy-todo-to-today` keeps working because it calls the same `org-journal-file-header` lambda (config.org:567).
