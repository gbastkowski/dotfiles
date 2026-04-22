## MODIFIED Requirements

### Requirement: Limit the MVP managed surface to `.p10k.zsh`
The system SHALL manage `~/.p10k.zsh` as the initial Home Manager file target, sourced from the repository-owned `zsh/.p10k.zsh`.
The managed surface is not limited to this file; subsequent changes MAY extend Home Manager management to additional dotfiles and external theme sources via flake inputs.

#### Scenario: p10k.zsh is managed by Home Manager
- **WHEN** the user inspects the Home Manager configuration
- **THEN** it manages `~/.p10k.zsh` sourced from `zsh/.p10k.zsh`
