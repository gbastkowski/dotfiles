{ lib, ... }:
{
  # Keep ~/.agents/skills and its lockfile writable by the skills CLI.
  # OpenCode reads this shared directory; Claude Code gets skill symlinks.
  home.activation.agentSkills =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      # Scope the extra tool dirs to this lookup, preserving activation's PATH.
      npx_bin="$(PATH="$PATH:/opt/homebrew/bin:/usr/local/bin:/usr/bin:$HOME/.local/bin" command -v npx || true)"
      if [ -z "$npx_bin" ]; then
        echo "npx not on PATH, skipping skill seeding" >&2
      else
        run "$npx_bin" --yes skills add vercel-labs/skills \
          -g -a claude-code opencode --skill find-skills -y \
          || echo "failed to install find-skills" >&2
      fi
    '';
}
