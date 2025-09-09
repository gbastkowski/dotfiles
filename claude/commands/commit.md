---
name: commit
description: Create a git commit following proper commit message guidelines
---

# Git Commit

Creates a git commit following the established commit message guidelines.

## Commit Message Guidelines Applied:
- Subject line: Keep concise (around 70 characters), capitalize first letter
- Use imperative mood ("Fix bug" not "Fixed bug")  
- Separate subject line and body with blank line when using body text
- Wrap body text to around 80-100 characters
- Prefix commit messages with the current ticket number when working on a Jira issue
- Do not add "generated with Claude Code" messages
- Never add co-authored lines to commit messages

## Process:
1. Review staged changes with `git status` and `git diff --staged`
2. Craft commit message following the guidelines above
3. Create commit with proper formatting