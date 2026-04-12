# Design: Task Refile to Today's Journal Function

## Context

**Current State:**
- Users manually navigate to today's journal file to move tasks
- No automated function exists for this common workflow operation
- Task organization across journal files is inconsistent
- Existing org-refile functionality requires manual target selection

**Constraints:**
- Must work with existing org-mode and Doom Emacs configuration
- Should preserve all task metadata (tags, properties, state, timestamps)
- Must handle cases where today's journal file doesn't exist
- Should integrate with existing org-capture workflow
- Must be performant even with large org files

**Stakeholders:**
- Primary: Users who manage daily tasks in org-mode journals
- Secondary: Users who use org-mode for time tracking and productivity
- Tertiary: Maintainers of the Doom Emacs configuration

## Goals / Non-Goals

**Goals:**
- Create a function that moves/refiles tasks to today's journal
- Automate journal file creation if it doesn't exist
- Preserve all task metadata during the operation
- Provide configurable behavior (copy vs move)
- Add convenient keybinding for quick access
- Maintain compatibility with existing org workflow

**Non-Goals:**
- Refactoring existing org-refile functionality
- Changing journal file naming conventions
- Adding GUI elements or complex interfaces
- Supporting non-journal file targets
- Modifying org-capture template structure
- Adding database or external storage integration

## Decisions

### Function Implementation Approach
**Decision:** Create a dedicated function `gunnar/org-copy-todo-to-today` rather than extending org-refile

**Rationale:**
- Dedicated function provides clearer intent and usage
- Avoids complexity of org-refile's target selection interface
- Allows for journal-specific optimizations and behavior
- Easier to document and maintain as a standalone feature

**Alternatives Considered:**
1. **Extend org-refile**: Would require complex target selection logic and lose the "today" specificity
2. **Use org-capture**: Would require capturing from existing tasks, losing original context
3. **Modify org-move-subtree**: Too low-level, would require manual file management

### Journal File Handling
**Decision:** Automatically create today's journal file if it doesn't exist

**Rationale:**
- Provides seamless user experience without manual file creation
- Follows principle of least surprise - function should "just work"
- Consistent with org-journal's automatic file creation behavior
- Reduces friction in daily workflow

**Implementation:**
- Use existing `gunnar/today-note-file-name` function for consistency
- Create file with proper org-mode headers if missing
- Ensure directory structure exists before file creation

### Task Metadata Preservation
**Decision:** Preserve all task metadata including properties, tags, and state

**Rationale:**
- Maintains task history and context
- Prevents data loss during refiling operations
- Ensures tasks remain functional after moving
- Follows org-mode best practices for task management

**Implementation:**
- Use org-copy-subtree for copy operations (preserves all metadata)
- Use org-cut-subtree for move operations (preserves all metadata)
- Ensure properties drawer is copied intact
- Maintain original task state and priority

### Copy vs Move Behavior
**Decision:** Make copy/move behavior configurable via prefix argument

**Rationale:**
- Provides flexibility for different use cases
- C-s (universal argument) is idiomatic in Emacs for behavior modification
- Allows users to choose based on context
- Maintains original task when experimentation is needed

**Implementation:**
- C-u prefix (universal argument) → copy task, keep original
- No prefix → move task, delete original
- Clear documentation of behavior difference

### Keybinding Location
**Decision:** Add keybinding in org-mode-map under leader key

**Rationale:**
- Leader key bindings are discoverable and consistent in Doom Emacs
- Org-mode-map ensures binding only active in org buffers
- Follows existing pattern for org-specific functions
- Avoids conflicts with global bindings

**Proposed Binding:** `\[leader] j t` (journal today)

## Risks / Trade-offs

### [Risk] Function conflicts with existing org workflow
**Mitigation:** 
- Thorough testing with various org file types
- Ensure compatibility with org-refile, org-capture, org-agenda
- Add comprehensive documentation and examples
- Make function behavior predictable and consistent

### [Risk] Performance issues with large org files
**Mitigation:**
- Use efficient org-element API for parsing
- Limit operations to necessary subtree only
- Test with files containing 10,000+ lines
- Add performance logging for debugging

### [Risk] Journal file naming inconsistencies
**Mitigation:**
- Reuse existing `gunnar/today-note-file-name` function
- Validate file naming convention matches existing journals
- Ensure consistent directory structure
- Document expected file naming pattern

### [Risk] Metadata loss during refiling
**Mitigation:**
- Use org's built-in copy/cut functions that preserve metadata
- Add validation to check metadata preservation
- Include comprehensive test cases for metadata
- Provide clear error messages if metadata is lost

## Migration Plan

**Deployment Steps:**
1. Add function to config.org with proper documentation
2. Add keybinding in org-mode-map
3. Test with various task types and scenarios
4. Update personal documentation/cheatsheet
5. Monitor usage and gather feedback

**Rollback Strategy:**
- Function is additive - can be removed without affecting existing functionality
- Keybinding can be easily disabled if needed
- No database migrations or data changes required
- Simple git revert if issues arise

## Open Questions

1. Should the function support refiling to arbitrary dates, or only today?
   - Current decision: Today only (simpler, covers 90% of use cases)
   - Future enhancement: Could add date selection if needed

2. Should there be a confirmation prompt for move operations?
   - Current decision: No confirmation (matches org-refile behavior)
   - Alternative: Add optional confirmation for safety

3. Should the function handle archived tasks differently?
   - Current decision: Treat archived tasks same as active tasks
   - Alternative: Skip or warn about archived tasks

4. Should there be a limit on task size/complexity?
   - Current decision: No artificial limits
   - Alternative: Add size warning for very large tasks

## Implementation Outline

1. **Core Function** (`gunnar/org-copy-todo-to-today`)
   - Determine today's journal file path
   - Check if file exists, create if needed
   - Copy or move task based on prefix argument
   - Preserve all metadata and structure
   - Handle errors gracefully

2. **Journal File Management**
   - Use existing naming conventions
   - Create directory structure if needed
   - Add proper org-mode headers
   - Ensure file is saved after creation

3. **Keybinding Integration**
   - Add to org-mode-map
   - Document in cheatsheet
   - Ensure no conflicts

4. **Testing and Validation**
   - Test with various task types
   - Verify metadata preservation
   - Test edge cases (empty journal, large tasks)
   - Performance testing

This design provides a solid foundation for implementing the task refile function while maintaining flexibility for future enhancements.