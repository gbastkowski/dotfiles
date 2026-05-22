## ADDED Requirements

### Requirement: Daily journal file header SHALL include a hybrid GTD + Atomic Habits scaffold

When `org-journal` creates a new daily journal file, the file header lambda `org-journal-file-header` SHALL emit, after the existing `#+TITLE` and `#+CATEGORY` lines, a structured org-mode scaffold containing the following content in order:

1. Two file-level keyword lines: `#+IDENTITY:` (empty value) and `#+ENERGY: L/M/H`.
2. A blank line.
3. `* Intention — the one thing` (level 1).
4. `* Habits :habits:` (level 1) with three concrete daily-habit checkboxes: `- [ ] Use pomodoro`, `- [ ] Eat apple`, `- [ ] Do exercise`.
5. `* Evening review` (level 1) containing two checkboxes (`- [ ] Inbox processed`, `- [ ] Habits ticked / scored`) and bullet stubs for `Wins:`, `Friction:`, `Tomorrow's one thing:`.

The lambda MUST return a single string (current contract). Template sections sit above org-journal's day heading, which org-journal appends below on the first time-stamped entry.

#### Scenario: New journal file is created via org-journal

- **WHEN** `org-journal-new-entry` creates a journal file that does not yet exist
- **THEN** the file content immediately after the `#+TITLE` and `#+CATEGORY` lines contains the `#+IDENTITY:` and `#+ENERGY:` keywords followed by the three template section headings (`* Intention — the one thing`, `* Habits :habits:`, `* Evening review`), in declared order, at level 1

#### Scenario: Journal file is auto-created by gunnar/org-copy-todo-to-today

- **WHEN** `gunnar/org-copy-todo-to-today` triggers creation of today's journal file (file did not exist) by invoking `org-journal-file-header`
- **THEN** the new file contains the same template content in the same order as a file created via `org-journal-new-entry`

#### Scenario: Existing journal file is opened

- **WHEN** an already-existing journal file from before this change is opened
- **THEN** its content is unchanged — the new template is not injected retroactively

### Requirement: Identity and energy SHALL be exposed as file-level keywords

The template SHALL declare `#+IDENTITY:` and `#+ENERGY:` as file-level org keyword lines (next to `#+TITLE` and `#+CATEGORY`), not as a property drawer attached to a heading. `#+IDENTITY:` SHALL be emitted with an empty value. `#+ENERGY:` SHALL be emitted with the placeholder value `L/M/H`.

#### Scenario: Keywords emitted in new file

- **WHEN** a new journal file is created
- **THEN** the file header contains a line `#+IDENTITY:` with empty value and a line `#+ENERGY: L/M/H`, both above the first `*` heading

### Requirement: Carryover of open TODOs SHALL continue to function

The existing `org-journal-carryover-items` behaviour (carrying entries matching `TODO="TODO|NEXT"` from the prior day) SHALL remain functional after the template is added. The template MUST NOT introduce headings or markers that prevent org-journal from locating the carryover insertion point.

#### Scenario: Open TODOs carry over into a new day with the template

- **WHEN** a new journal file is created on a day after one that contained open `TODO` or `NEXT` items
- **THEN** those items appear in the new file alongside the template scaffold, and the template sections remain intact
