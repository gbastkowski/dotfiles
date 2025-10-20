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
- A jira issue with digits (e.g. 1234) only usually refers to the precise issue key SQDD06-1234

# Git and Gitlab Workflow

## General Rules
- Prefer one-line git commit messages
- Prefix with the current ticket number
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