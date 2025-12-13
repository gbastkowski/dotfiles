---
name: create-jira-story
description: Create Jira stories, tasks, and bugs in the SQDD06 project with proper formatting. Use when the user wants to create new Jira issues with summaries, descriptions, acceptance criteria, or additional information.
allowed-tools: mcp__jira-confluence__create_jira_issue
---

# Create Jira Story

Creates well-structured Jira stories, tasks, and bugs in the SQDD06 project following RDM team conventions.

## When to Use This Skill

Use this skill when:
- User asks to create a new Jira story, task, or bug
- Converting requirements or discussions into Jira issues
- Need to document features with acceptance criteria
- Creating issues with proper ISTA/RDM formatting

## Jira Story Writing Guidelines

### Summary Format
- Keep summaries short and specific
- Focus on the concrete scope
- Use official tool names (e.g., "sbt" not "SBT")
- Good: "Change sbt-based release process in base-settings"
- Bad: "Change SBT-based release process to avoid version commits and use git tags as single source of truth"

### Description Structure
- Add explicit scope-limiting paragraph when applicable
  Example: "This story is only about changing X in Y. When this works well we can roll out that process to other projects."
- Use heading level 3 (###) for section headers, not bold text
- Common sections: `### Current Problems`, `### Proposed Solution`, `### References`

### Acceptance Criteria
- Use heading level 3 (###) for each criterion
- Format GIVEN/WHEN/THEN scenarios as single bullet points with line breaks:
  ```
  - **_given_** a condition  
    **_when_** I do something  
    **_then_** result happens
  ```
- Keep criteria focused and essential
- Prefer simple, verifiable criteria over detailed planning
- Remove overly detailed investigation plans, test project lists, and success metrics
- For documentation criteria, keep it simple: "### Documentation" with "A few sentences in the readme"

### Additional Information
- Keep minimal - only truly important supplementary information
- "Out of Scope" section only if is useful to include or explicitly asked for
- Avoid detailed investigation plans, test project lists, success metrics

### Overall Philosophy
- Prefer lean, focused stories over detailed upfront planning
- Stories should be clear about scope and acceptance, but not overspecified
- Let implementation details emerge during work rather than planning everything in the issue

## Available Issue Types

The SQDD06 project supports:
- **Story** (id: 26): User-facing features and enhancements
- **Task** (id: 39): Implementation work
- **Bug** (id: 1): Issues to fix
- **Change** (id: 10002): Release/deployment changes
- **Service Request** (id: 12100): External requests
- **Subtask** (id: 36): Sub-tasks of other issues
- **Epic** (id: 34): Large initiatives

## Custom Fields

- **Acceptance Criteria** (customfield_11102): GIVEN/WHEN/THEN format
- **Additional Information** (customfield_27806): Investigation notes, out of scope, references

## Example: Simple Story

```
Summary: Add JQL search to Jira MCP server

Description:
### Current State
The MCP server can fetch individual issues but cannot search for multiple issues.

### Proposed Solution
Add a search_jira_issues tool that accepts JQL queries.

Acceptance Criteria:
### Basic Search
- GIVEN a JQL query string
  WHEN I call search_jira_issues
  THEN return matching issues with key, summary, status, and type

### Pagination
- GIVEN a search with many results
  WHEN I specify maxResults and startAt
  THEN return paginated results with isLast indicator

Additional Information:
### References
JQL documentation: https://docs.atlassian.com/jira/jira-software/server/JQL-syntax
```

## Example: Story with Scope Limiting

```
Summary: Support ADF hard line breaks in markdown converter

Description:
This story is only about adding hard line break support to the markdown-to-ADF converter.
When this works well we can extend ADF support to other tools.

### Current Problem
Markdown hard line breaks (two spaces or backslash at end of line) are not converted to ADF hardBreak nodes.

### Proposed Solution
Detect hard line break patterns and emit hardBreak nodes in ADF output.

Acceptance Criteria:
### Two-Space Breaks
- GIVEN markdown text with two spaces at end of line
  WHEN converting to ADF
  THEN emit hardBreak node in ADF

### Backslash Breaks
- GIVEN markdown text with backslash at end of line
  WHEN converting to ADF
  THEN emit hardBreak node in ADF

### Documentation
A few sentences in the readme explaining the supported markdown syntax.

Additional Information:
### Out of Scope
Other markdown extensions beyond hard breaks will be handled in future stories.
```

## Workflow

When creating a Jira issue:
1. Gather all necessary information (summary, description, criteria)
2. Format according to RDM conventions
3. Use mcp__jira-confluence__create_jira_issue tool
4. Provide the Jira issue URL once created

## References

- Jira URL: https://ista.atlassian.net
- Default project: SQDD06
- Team conventions are documented in CLAUDE.md
