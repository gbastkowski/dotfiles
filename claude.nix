{ ... }:
{
  home.file.".claude/CLAUDE.md".source = ./claude/CLAUDE.md;
  home.file.".claude/settings.json".source = ./claude/settings.json;
  home.file.".claude/hooks/claude-atuin-record.sh" = { source = ./claude/hooks/claude-atuin-record.sh; executable = true; };
  home.file.".claude/statusline.sh".source = ./claude/statusline.sh;
  home.file.".claude/commands".source = ./claude/commands;
  home.file.".claude/agents".source = ./claude/agents;
}
