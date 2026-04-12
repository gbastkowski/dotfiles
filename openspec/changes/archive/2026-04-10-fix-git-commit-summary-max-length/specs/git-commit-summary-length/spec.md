## ADDED Requirements

### Requirement: Configured commit summary length is honored
The Emacs configuration SHALL apply the configured `git-commit-summary-max-length` value to git commit message editing so that the active summary length behavior matches the configured limit at runtime.

#### Scenario: Commit buffer uses configured summary length
- **WHEN** a user opens a git commit message buffer from the configured Emacs environment
- **THEN** the effective commit summary length behavior SHALL use the configured `git-commit-summary-max-length` value instead of falling back to unrelated global editing width settings

### Requirement: Commit-specific width stays local
The system SHALL scope the commit summary length fix to git commit editing so that unrelated buffers continue using their existing global editing width configuration.

#### Scenario: Non-commit buffers keep global width behavior
- **WHEN** the commit summary length fix is active
- **THEN** non-commit editing buffers SHALL continue to use the existing global `fill-column` behavior unless they already define their own buffer-local settings

### Requirement: Literate configuration remains authoritative
The system SHALL implement the fix in the literate Emacs configuration source and propagate it to generated configuration output so the configured commit summary length does not drift between source and runtime files.

#### Scenario: Generated config matches literate source
- **WHEN** the git commit summary length fix is updated in `config.org`
- **THEN** the generated Emacs configuration SHALL reflect the same effective commit summary length behavior after regeneration
