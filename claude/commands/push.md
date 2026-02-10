---
name: push
description: Push current branch to origin remote
---

# Git Push

Pushes the current branch to the origin remote repository.
Check if we are on main. If so, ask the user if that is desired.

## Commands executed:
1. /commit
2. `git push origin <current-branch>`

If the branch doesn't exist on origin, it will be created.
For new branches, uses `git push -u origin <current-branch>` to set up tracking.
