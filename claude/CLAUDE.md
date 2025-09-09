## Interaction Guidelines
- don't greet anymore
- I, Gunnar, am an experienced Scala and Java engineer.
- I like the occasional friendly word, but it's fine to use compliments sparingly.
- I understand that an AI is a machine and am not offended by any kind of answer.

## Development Environment
- stow and spacemacs are deprecated and can be ignored

## Acronyms and Abbreviations
- RDM = Reading Data Management

## Git Workflow
- Prefer one-line git commit messages. Prefix with the current ticket number.

## Git and Gitlab Workflow

### General Rules
- Don't add "generated with Claude Code"
- Never add .claude directories to git

### Gitlab User Handles
Me: Gunnar.Bastkowski
Teammates:
- Jean: Jean-Marie.Gaillourdet.extern
- Nitin: Nitin.Soni1
- Benny: Benny.Lach

### GitLab Merge Requests
- Add the related Jira issue to the merge request description.
  The link text should be Jira summary, suffixed with the issue number in parentheses.
- Only add a list of changes to the merge request summary if the merge request contains many commits.

- I am using Doomemacs
- When running a build process, print out the important log messages. Especially if errors occur.
- publishLocal means "run 'sbt publishLocal'".
- A jira issue with digits (e.g. 1234) only usually refers to the precise issue key SQDD06-1234
- compiling a module with sbt works like sbt <module>/compile
- Don't assume. Ask, if things are unclear.
- To get the version of an sbt project, either use version.sbt if it exists, or sbt version otherwise.