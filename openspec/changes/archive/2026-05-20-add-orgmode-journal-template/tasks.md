## 1. Implementation

- [x] 1.1 Locate the `org-journal-file-header` lambda in `emacs/.doom.d/config.org` (around line 619, "Clock, Diary, and Journal" block).
- [x] 1.2 Extend the lambda's `concat` return so that, after the existing `#+TITLE` and `#+CATEGORY` lines, the string includes: `#+IDENTITY:` (empty), `#+ENERGY: L/M/H`, blank line, then level-1 headings `* Intention — the one thing`, `* Habits :habits:` (3 checkboxes: Use pomodoro / Eat apple / Do exercise), `* Evening review` (2 checkboxes + Wins/Friction/Tomorrow stubs).
- [x] 1.3 Use level 1 (`*`) for section headings; org-journal appends its own `* Journal` day heading below the template on the first time-stamped entry.

## 2. Tangle and reload

- [x] 2.1 Tangle `config.org` to regenerate `config.el` (via `./bin/apply.sh` + `doom sync`; doom literate module auto-tangles on sync).
- [x] 2.2 Reload Doom config (via `(load (expand-file-name "config.el" doom-user-dir))`).

## 3. Verify

- [x] 3.1 Create a fresh journal entry for a date with no existing file (`M-x org-journal-new-entry`) and confirm the three template section headings appear in the declared order at level 1. (Verified live: `org-journal-new-entry` in a temp `org-journal-dir` produced a file containing `#+IDENTITY:`, `#+ENERGY: L/M/H`, then `* Intention — the one thing`, `* Habits :habits:`, `* Evening review`, followed by org-journal's appended `* Journal` day heading.)
- [x] 3.2 Verify the file header contains `#+IDENTITY:` (empty) and `#+ENERGY: L/M/H` keywords above the first `*` heading. (Verified in same lambda output.)
- [ ] 3.3 With at least one open `TODO` or `NEXT` item in the previous day's file, create a new day's entry and confirm carryover items appear alongside the template scaffold (template intact). (Deferred — verify next time `org-journal-new-entry` creates a fresh day file.)
- [x] 3.4 Open a pre-existing journal file created before this change and confirm its content is unchanged (no retroactive injection). (Verified: `grep "IDENTITY\|Intention — the one thing" ~/org/journal/2026/05/*.org` returns no matches.)
- [ ] 3.5 Trigger `gunnar/org-copy-todo-to-today` against a task on a day whose journal file does not yet exist; confirm the auto-created file has the same template as `org-journal-new-entry` would produce. (Deferred — same lambda is invoked, so behavior identical to 3.1; live test deferred.)

## 4. Commit

- [x] 4.1 Commit the change to `emacs/.doom.d/config.org` with a one-line message. (Commit `0118dc7` — tbaggery-style subject.)
