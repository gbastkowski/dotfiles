# Proposal: Find or Create Function to Move/Refile a Task to Today's Journal

## Why

This change addresses the need for efficient task management in org-mode workflows.
Currently, moving tasks to today's journal requires manual file navigation and refiling, which disrupts workflow efficiency.
A dedicated function would streamline this common operation, saving time and reducing cognitive load when organizing daily tasks.

The function will solve these specific problems:
- Manual navigation to today's journal file is tedious and repetitive
- No consistent way to move tasks to the current day's journal
- Lack of automation for a common daily workflow operation
- Inconsistent task organization across different journal files

## What Changes

This change will add a new function to the Emacs configuration that:

- **Creates a new function** `gunnar/org-copy-todo-to-today` that:
  - Copies or moves tasks to today's journal file
  - Creates today's journal file if it doesn't exist
  - Places tasks under a "Tasks" heading in the journal
  - Preserves task metadata (tags, properties, state)
  - Provides option to keep or delete the original task

- **Adds keybinding** for quick access to the function
- **Enhances existing org workflow** without breaking current functionality
- **Improves task organization** consistency across journal files

## Capabilities

### New Capabilities

- **task-refile-to-today**: Functionality to move/refile tasks to the current day's journal
  - Handles journal file creation if needed
  - Maintains task structure and metadata
  - Provides user configurable behavior (copy vs move)
  - Integrates with existing org-capture workflow

### Modified Capabilities

None. This change introduces new functionality without modifying existing capabilities.

## Impact

**Affected code:**
- `emacs/.doom.d/config.org` - Main configuration file
- User's daily org workflow and task management process

**Dependencies:**
- Existing org-mode functionality (org-refile, org-capture)
- Current journal file structure and naming conventions
- Doom Emacs org configuration

**Benefits:**
- **Time savings**: Reduces manual steps for common task organization
- **Consistency**: Ensures tasks are properly organized in daily journals
- **Workflow improvement**: Streamlines daily planning and task management
- **Reduced friction**: Makes it easier to maintain organized task tracking

**Risks/Mitigations:**
- **Integration with existing workflow**: Function will be additive, not disruptive
- **File structure assumptions**: Will use existing journal naming conventions
- **User adoption**: Clear documentation and keybinding will encourage usage

## Success Criteria

1. Function successfully moves tasks to today's journal file
2. Journal file is automatically created if missing
3. Task metadata (tags, properties, state) is preserved
4. User can choose between copy and move behavior
5. Function integrates smoothly with existing org workflow
6. Keybinding provides quick access to the functionality

## Out of Scope

- Refactoring existing org configuration
- Changing journal file naming conventions
- Modifying org-capture templates
- Adding GUI elements or complex interfaces
- Supporting non-journal file targets

## Proposed Implementation Approach

1. Create the core function with configurable behavior
2. Add appropriate keybinding in org-mode
3. Test with various task types and metadata
4. Document usage and behavior
5. Integrate with existing org workflow patterns

This change aligns with the principle of automating repetitive tasks while maintaining the flexibility and power of org-mode's task management system.
