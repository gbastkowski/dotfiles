## Why

Copying a selected Org region into Markdown for external tools is manual and interruptive today.
This change introduces a single command to export the active region to Markdown and place it on the system clipboard.

## What Changes

- Add an interactive Emacs Lisp function to convert the active Org region to Markdown and yank it to the system clipboard.
- Provide a clear user-facing behavior when no region is active (e.g., error message).
- Limit the initial scope to org-mode regions only.

## Capabilities

### New Capabilities
- `org-region-markdown-clipboard`: Export the active Org region to Markdown and copy it to the system clipboard via an interactive command.

### Modified Capabilities

## Impact

- Emacs Lisp configuration under `emacs/`.
- Uses Org export facilities and clipboard integration.
