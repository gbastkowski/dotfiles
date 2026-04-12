# Task Refile to Today Specification

## ADDED Requirements

### Requirement: Task refile to today's journal
The system SHALL provide a function to move or copy tasks to the current day's journal file.

#### Scenario: Move task to today's journal
- **WHEN** user calls `gunnar/org-copy-todo-to-today` on a task without prefix argument
- **THEN** system moves the task to today's journal file under "Tasks" heading
- **AND** original task is removed from source location
- **AND** all task metadata is preserved

#### Scenario: Copy task to today's journal
- **WHEN** user calls `gunnar/org-copy-todo-to-today` with prefix argument (C-u)
- **THEN** system copies the task to today's journal file under "Tasks" heading
- **AND** original task remains in source location
- **AND** all task metadata is preserved in the copy

#### Scenario: Automatic journal file creation
- **WHEN** today's journal file does not exist
- **THEN** system creates the journal file with proper structure
- **AND** file includes standard org-mode headers
- **AND** file is saved in the correct journal directory

#### Scenario: Metadata preservation
- **WHEN** task is moved or copied
- **THEN** system preserves task properties, tags, state, and priority
- **AND** original task structure is maintained
- **AND** timestamps are preserved if applicable

#### Scenario: Error handling for invalid tasks
- **WHEN** function is called on non-task content
- **THEN** system displays clear error message
- **AND** no changes are made to any files

#### Scenario: Keybinding access
- **WHEN** user is in org-mode
- **THEN** function is accessible via configured keybinding
- **AND** keybinding follows Doom Emacs conventions

## MODIFIED Requirements

None. This change introduces new functionality without modifying existing requirements.

## REMOVED Requirements

None. No existing functionality is being deprecated or removed.

## RENAMED Requirements

None. No requirement names are being changed.