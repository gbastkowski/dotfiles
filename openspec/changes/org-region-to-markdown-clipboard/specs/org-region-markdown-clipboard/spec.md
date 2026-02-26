## ADDED Requirements

### Requirement: Export active Org region to Markdown
The system SHALL provide an interactive command that converts the active Org region to Markdown and yanks the result to the system clipboard.

#### Scenario: Export succeeds with an active region
- **WHEN** an Org buffer has an active region and the command is invoked
- **THEN** the region content is converted to Markdown and placed on the system clipboard

### Requirement: Guard against missing region
The system MUST report a user-facing error when the command is invoked without an active region and MUST NOT change the clipboard.

#### Scenario: No active region
- **WHEN** the command is invoked without an active region
- **THEN** an error is shown and the clipboard content is unchanged

### Requirement: Limit scope to org-mode
The system MUST only operate in org-mode buffers and MUST report a user-facing error if invoked elsewhere.

#### Scenario: Command invoked outside org-mode
- **WHEN** the command is invoked in a non-org buffer
- **THEN** an error is shown and no Markdown export is attempted

### Requirement: No keybinding required
The system SHALL provide the command without requiring a default keybinding.

#### Scenario: Command available without a keybinding
- **WHEN** the user inspects the available commands
- **THEN** the command exists and no default keybinding is installed
