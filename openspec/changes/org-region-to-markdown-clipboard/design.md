## Context

The change adds a small Emacs Lisp command in the dotfiles to export an active Org region to Markdown and copy it to the system clipboard.
Initial scope is limited to org-mode regions and a single interactive command.

## Goals / Non-Goals

**Goals:**
- Provide a fast command that converts the active Org region to Markdown and yanks it to the system clipboard.
- Give a clear error when no region is active.
- Keep the implementation localized to Emacs configuration without external services.

**Non-Goals:**
- Exporting non-Org buffers or whole documents.
- Custom Markdown styling or complex export backends beyond the default Org Markdown exporter.
- Cross-editor support.

## Decisions

- Use the Org Markdown exporter (`ox-md`) via `org-export-string-as` on the region text.
  - Alternatives considered: `org-md-export-to-markdown` (file-based) and `org-export-as` (buffer-based) were heavier for a region-only command.
- Use `buffer-substring-no-properties` to capture only the plain region text.
  - Alternative: `buffer-substring` would include text properties that are irrelevant for export.
- Use `kill-new` for clipboard integration, relying on Emacs clipboard settings (`select-enable-clipboard`).
  - Alternative: `gui-set-selection` or external clipboard tools were avoided to keep the change portable.

## Risks / Trade-offs

- `ox-md` may not be loaded by default. → Mitigation: explicitly `(require 'ox-md)` in the command.
- Clipboard behavior depends on Emacs settings or OS integration. → Mitigation: rely on `kill-new` and document expectations in the code comments if needed.
- Markdown output may differ from external tools' expectations. → Mitigation: keep scope simple and adjust exporter options later if needed.

## Migration Plan

- Add the command to the Emacs config and reload.
- No data migration or rollback steps required; remove the function if needed.

## Open Questions

- Where should the function live in the Emacs config (org-specific file vs general utilities)?
- Should a keybinding be added now or left to the user?
