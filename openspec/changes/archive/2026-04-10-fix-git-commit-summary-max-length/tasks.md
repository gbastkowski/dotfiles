## 1. Update git commit configuration

- [x] 1.1 Inspect the existing `Git/Magit Settings` section in `emacs/.doom.d/config.org` and identify the narrowest git-commit-specific hook or setup point for buffer-local summary width behavior.
- [x] 1.2 Update `emacs/.doom.d/config.org` so git commit buffers honor the configured `git-commit-summary-max-length` value at runtime without changing the global `fill-column` setting.

## 2. Regenerate and verify configuration output

- [x] 2.1 Regenerate `emacs/.doom.d/config.el` from `config.org` so the generated config matches the literate source.
- [x] 2.2 Verify the Emacs configuration is syntactically valid and that the git commit summary length behavior is scoped to commit buffers rather than unrelated editing buffers.
