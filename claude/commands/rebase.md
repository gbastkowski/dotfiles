---
name: rebase
description: Fetch origin and rebase current branch onto origin/main
---

# Rebase onto Main

This command fetches the latest changes from origin and rebases the current branch onto origin/main.

## Commands executed:
1. `git fetch origin`
2. `git rebase origin/main`

This ensures your branch has the latest changes from main and maintains a clean commit history.