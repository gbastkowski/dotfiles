# Interaction Guidelines
- don't greet anymore
- I, Gunnar, am an experienced Scala and Java engineer
- I like the occasional friendly word, but it's fine to use compliments sparingly
- I understand that an AI is a machine and am not offended by any kind of answer
- Keep responses brief, avoid repeating information

# Writing/Coding Styleguide
- For markdown, org-mode and similar formats: sentences should start on new lines

# Acronyms and Abbreviations
- RDM = Reading Data Management

# Development Environment
- I am using Doomemacs
- When running a build process, print out the important log messages. Especially if errors occur.
- Don't assume. Ask, if things are unclear.

# Installed Tools
- `gh` - GitHub CLI for repository management
- `glab` - GitLab CLI for repository management
- `jira` - Jira CLI for issue management

# SBT and Scala
- publishLocal means "run 'sbt publishLocal'"
- compiling a module with sbt works like sbt <module>/compile
- To get the version of an sbt project, either use version.sbt if it exists, or sbt version otherwise

# Jira

## Project Configuration
- Default project: SQDD06
- Jira URL: https://ista.atlassian.net
- Config file: ~/.config/.jira/.config.yml
- A jira issue with digits (e.g. 1234) only usually refers to the precise issue key SQDD06-1234

## Available Issue Types
The SQDD06 project has the following issue types:
- Epic (id: 34)
- Story (id: 26)
- Task (id: 39)
- Change (id: 10002)
- Service Request (id: 12100)
- Bug (id: 1)
- Subtask (id: 36)
- Objective (id: 15801)
- KR (id: 15400)
- KPI (id: 19200)
- Emergency User Request (id: 18300)
- Risk (id: 11300)
- Impediment (id: 41)

## Custom Fields
Stories have the following custom fields configured:
- **Acceptance Criteria** (customfield_11102): Use `--custom acceptance-criteria="text"`
- **Additional Information** (customfield_27806): Use `--custom additional-information="text"`

## Creating Issues with jira-cli
```bash
# Create a story with custom fields
jira issue create -t Story -s "Title" \
  -b "Description" \
  --custom acceptance-criteria="Given...When...Then..." \
  --custom additional-information="Investigation notes, test plans, etc."

# Create a task
jira issue create -t Task -s "Title" -b "Description"

# Use --web flag to open the created issue in browser immediately
jira issue create -t Story -s "Title" --web
```

## Jira Issue Writing Preferences

### Summary
- Keep summaries short and specific
- Focus on the concrete scope (e.g., "Change sbt-based release process in base-settings" not "Change SBT-based release process to avoid version commits and use git tags as single source of truth")
- Use official tool names: "sbt" not "SBT"

### Description Structure
- Add explicit scope-limiting paragraph when applicable (e.g., "This story is only about changing X in Y. When this works well we can roll out that process to other projects.")
- Use heading level 3 (###) for section headers, not bold text (**Section:**)
- Examples: `### Current Problems`, `### Proposed Solution`, `### References`

### Acceptance Criteria
- Use heading level 3 (###) for each criterion
- Format GIVEN/WHEN/THEN scenarios as single bullet points with line breaks, not separate bullets
  ```
  - GIVEN a condition
    WHEN I do something
    THEN result happens
  ```
- Keep criteria focused and essential
- Prefer simple, verifiable criteria over detailed planning
- Remove overly detailed investigation plans, test project lists, and success metrics from acceptance criteria
- For documentation criteria, keep it simple: "### Documentation" with "A few sentences in the readme"

### Additional Information
- Keep minimal - only truly important supplementary information
- "Out of Scope" section is useful to include
- Avoid detailed investigation plans, test project lists, success metrics in the issue itself (handle these during implementation)

### Overall Philosophy
- Prefer lean, focused stories over detailed upfront planning in the issue
- Stories should be clear about scope and acceptance, but not overspecified
- Let implementation details emerge during work rather than planning everything in the issue

# Git and Gitlab Workflow

## General Rules
- Prefer one-line git commit messages
- Prefix with the current ticket number only if a ticket number has been mentioned in the conversation
- Don't add "generated with Claude Code"

## Gitlab User Handles
Me: Gunnar.Bastkowski
Teammates:
- Jean: Jean-Marie.Gaillourdet.extern
- Nitin: Nitin.Soni1
- Benny: Benny.Lach

## GitLab Merge Requests
- Add the related Jira issue to the merge request description.
  The link text should be Jira summary, suffixed with the issue number in parentheses.
- Only add a list of changes to the merge request summary if the merge request contains many commits.

# Database Access (RDM)
- Use `rdmctl postgres psql <env> <database>` to connect to databases
- Common databases: `output-channel-mdr-rgc`, `receiver-customerselfreadings-rgc`, `errors`, etc.
- Environment aliases: `wpro` (prod), `wpre` (preprod), `w devtest perf` (perf)
- Always switch to correct environment context before running rdmctl commands
- Use tmux windows for parallel database operations across environments
- Example: `rdmctl postgres psql prod output-channel-mdr-rgc -c "SELECT count(*) FROM table_name;"`
- Commands to read jira and web searches are always approved
- The glab command to get the logs of a job is glab ci trace <job id>
