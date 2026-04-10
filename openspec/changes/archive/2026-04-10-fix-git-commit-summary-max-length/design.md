## Context

The current Emacs configuration sets `git-commit-summary-max-length` to `70` in the literate config, but the runtime editing experience for commit messages does not consistently reflect that value.
In the same configuration, `fill-column` is globally set to `120`, and there is no repo-local `git-commit` or Magit mode hook that makes commit buffers adopt the customized summary width.
The result is a mismatch between the declared git commit summary limit and the behavior users see while writing commit messages.

## Goals / Non-Goals

**Goals:**
- Make the configured git commit summary max length active in commit message buffers.
- Keep the fix aligned with the literate `config.org` source of truth.
- Scope the change to git commit editing behavior without disturbing unrelated Emacs editing settings.

**Non-Goals:**
- Changing the global `fill-column` setting for the rest of the editor.
- Redesigning the broader Magit or git-commit workflow.
- Introducing new packages or non-local workarounds for a configuration issue.

## Decisions

### Keep `config.org` as the source of truth

The repo explicitly treats `config.el` as generated from `config.org`, so the fix should be made in the literate source and then propagated to the generated file.

**Alternative considered:** editing `config.el` directly.

That was rejected because it would drift from the documented configuration workflow and make the next tangle overwrite the fix.

### Configure commit buffers locally instead of changing global editor width

The bug is specific to commit message editing.
The design will preserve the global `fill-column` of `120` and add git-commit-specific setup so commit buffers use the customized summary width at runtime.

**Alternative considered:** lowering global `fill-column` to match the commit summary limit.

That was rejected because it would change unrelated editing behavior across the rest of the editor.

### Use package-local git-commit setup rather than relying on a passive variable assignment

The current configuration sets `git-commit-summary-max-length`, but that alone is not sufficient to guarantee that the active commit buffer layout or editing constraints reflect the intended value.
The implementation should add a local git-commit configuration hook or equivalent package-scoped setup so the effective runtime behavior is explicit.

**Alternative considered:** keeping only the existing variable assignment and assuming upstream defaults will honor it.

That was rejected because the observed bug is precisely that the configured value is not active in practice.

## Risks / Trade-offs

- **[Risk]** The chosen git-commit hook may influence more than just the summary line if configured too broadly. → **Mitigation:** keep the hook focused on commit-message-local width behavior only.
- **[Risk]** The generated `config.el` may drift if the fix is applied only in one place. → **Mitigation:** implement in `config.org` and regenerate `config.el` immediately afterward.
- **[Risk]** Runtime behavior may differ between summary validation and visual editing cues. → **Mitigation:** verify both the configured summary limit and the actual commit buffer-local width after the change.
