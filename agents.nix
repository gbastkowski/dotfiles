{ lib, ... }:
{
  # Keep ~/.agents/skills and its lockfile writable by the skills CLI.
  # OpenCode reads this shared directory; Claude Code gets skill symlinks.
  home.activation.agentSkills =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      # Keep npx and its node interpreter on PATH without changing activation's PATH.
      skills_path="$PATH:/opt/homebrew/bin:/usr/local/bin:/usr/bin:$HOME/.local/bin"
      npx_bin="$(PATH="$skills_path" command -v npx || true)"
      if [ -z "$npx_bin" ]; then
        echo "npx not on PATH, skipping skill seeding" >&2
      else
        run env PATH="$skills_path" "$npx_bin" --yes skills add vercel-labs/skills \
          -g -a claude-code opencode --skill find-skills -y \
          || echo "failed to install find-skills" >&2
      fi
    '';
}
