# Jira Assistant - Claude Code Plugin

Jira integration plugin for Claude Code that provides:
- Autonomous skill for creating well-formatted Jira stories, tasks, and bugs
- MCP server for comprehensive Jira and Confluence interaction
- ISTA RDM team conventions and formatting

## Features

### Jira Story Creation Skill
Claude autonomously creates Jira issues following RDM team conventions:
- Short, specific summaries focused on concrete scope
- Structured descriptions with markdown headings
- Acceptance criteria in GIVEN/WHEN/THEN format
- Minimal, focused additional information

### MCP Server Tools
Via the `@rdm/mcp-jira-confluence` MCP server:
- **get_jira_issue**: Fetch issue details including custom fields
- **add_jira_comment**: Add comments with markdown support
- **update_jira_comment**: Update existing comments
- **update_jira_description**: Update issue descriptions
- **update_jira_acceptance_criteria**: Update acceptance criteria
- **delete_jira_comment**: Delete comments
- **create_jira_issue**: Create stories, tasks, bugs with custom fields
- **add_jira_issue_link**: Link issues together
- **delete_jira_issue_link**: Remove issue links
- **search_jira_issues**: Search using JQL
- **get_jira_transitions**: Get available workflow transitions
- **transition_jira_issue**: Move issues through workflow
- **confluence_search_by_cql**: Search Confluence using CQL
- **confluence_search_by_title**: Search by page title
- **confluence_search_by_text**: Search page content
- **get_confluence_page**: Fetch page content
- **get_confluence_page_comments**: Fetch page comments

## Prerequisites

- Claude Code installed
- Access to ISTA Jira (https://ista.atlassian.net)
- Jira API token
- Access to ISTA Artifactory (for @rdm packages)

## Installation

### 1. Configure Artifactory Access

The plugin uses the `@rdm/mcp-jira-confluence` package from ISTA Artifactory.
Configure npm to access the private registry:

```bash
# If using CAS Artifactory configuration
# The npm config should already be set up via your .npmrc

# Or manually configure:
npm config set @rdm:registry https://ista.jfrog.io/artifactory/api/npm/cas-rdm-npm-private/
# Add authentication as needed
```

### 2. Install the Plugin

Option A: Install from local path (development):
```bash
/plugin install /path/to/jira-assistant-claude-plugin
```

Option B: Install from git repository:
```bash
/plugin install https://gitlab.com/ista-se/cas/rdm/ai/jira-assistant-claude-plugin
```

### 3. Configure Environment Variables

Set your Jira email in your environment or Claude Code settings:

```bash
export JIRA_EMAIL="your.email@ista.com"
```

Optional: Configure custom token command (defaults to `pass show jira-api-token`):
```bash
export JIRA_TOKEN_COMMAND="your-custom-command"
```

### 4. Restart Claude Code

After installation, restart Claude Code for the plugin to take effect.

## Usage

### Creating Jira Stories

Simply ask Claude to create a story:

```
Create a story for adding support for issue transitions in the Jira MCP server
```

Claude will:
1. Gather necessary information
2. Format according to RDM conventions
3. Create the issue using the skill
4. Provide the Jira issue URL

### Example Interactions

**Simple story:**
```
Create a task to add unit tests for the markdown-to-ADF converter
```

**Story with details:**
```
Create a story for supporting nested lists in markdown conversion.
It should handle at least 3 levels of nesting.
```

**Bug report:**
```
Create a bug: the search_jira_issues tool doesn't handle pagination correctly
```

### Using MCP Tools Directly

You can also ask Claude to use the Jira/Confluence tools directly:

```
Search for all open issues assigned to me in SQDD06

Get the details of SQDD06-4089

Add a comment to SQDD06-4089 saying the implementation is complete

Search Confluence for "MCP server" documentation
```

## Story Format Conventions

The skill follows these RDM team conventions:

### Summary
- Short and specific
- Focus on concrete scope
- Example: "Add JQL search to Jira MCP server"

### Description
- Use `### Heading` for sections
- Add scope-limiting paragraph when applicable
- Common sections: Current Problems, Proposed Solution, References

### Acceptance Criteria
- Use `### Criterion Name` for each criterion
- Format GIVEN/WHEN/THEN as single bullet with line breaks:
  ```
  - GIVEN a condition
    WHEN I do something
    THEN result happens
  ```

### Additional Information
- Keep minimal
- Include "Out of Scope" section when relevant
- References and links

## Configuration

### MCP Server Configuration

The plugin is pre-configured for ISTA Jira and Confluence:
- Jira: https://ista.atlassian.net
- Confluence: https://ista.atlassian.net/wiki

If you need to override these, modify `.mcp.json` in the plugin directory.

### Available Issue Types

The skill works with SQDD06 project issue types:
- Story (user-facing features)
- Task (implementation work)
- Bug (issues to fix)
- Change (release/deployment)
- Service Request (external requests)

## Troubleshooting

### Plugin not loading
- Ensure Claude Code is restarted after installation
- Check that Artifactory npm credentials are configured
- Verify JIRA_EMAIL environment variable is set

### MCP server fails to start
- Check that `pass show jira-api-token` returns a valid token
- Or set JIRA_TOKEN_COMMAND to your token retrieval method

### Cannot create issues
- Verify you have permissions in the SQDD06 project
- Check that your Jira API token is valid

## Development

### Project Structure

```
jira-assistant-claude-plugin/
├── .claude-plugin/
│   └── plugin.json              # Plugin manifest
├── skills/
│   └── create-jira-story/
│       └── SKILL.md             # Story creation skill
├── .mcp.json                    # MCP server configuration
└── README.md                    # This file
```

### Testing Locally

1. Clone the repository
2. Install from local path: `/plugin install /path/to/jira-assistant-claude-plugin`
3. Make changes to skill or configuration
4. Reload: `/plugin reload jira-assistant`

## License

UNLICENSED - Internal use by ISTA RDM team only.

## Support

For issues or questions:
- Open an issue in the GitLab repository
- Contact the RDM team
