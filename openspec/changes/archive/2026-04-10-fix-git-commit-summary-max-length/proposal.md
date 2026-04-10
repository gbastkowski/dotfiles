## Why

The Emacs configuration sets a customized git commit summary max length in `config.org`, but the effective runtime behavior still uses a different value.
This is a bug because the configured value is not actually honored, which makes commit message validation inconsistent with the user-owned configuration.

## What Changes

- Fix the Emacs git commit workflow so the configured commit summary max length from the literate config is actually active at runtime.
- Ensure the effective max length used by commit message editing and validation matches the customized configuration value.
- Keep the fix scoped to the configuration bug rather than changing unrelated git or Magit behavior.

## Capabilities

### New Capabilities
- `git-commit-summary-length`: Ensure Emacs honors the configured maximum git commit summary length during commit message editing and validation.

### Modified Capabilities

## Impact

- Affected code: `emacs/.doom.d/config.org` and any generated Emacs config derived from it.
- Affected behavior: git commit message editing and summary length validation in Emacs/Magit.
- Dependencies: existing Emacs git/Magit configuration only; no external systems or APIs.
