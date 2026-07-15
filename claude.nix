{ config, lib, pkgs, ... }:

let
  # Declarative MCP servers, seeded into ~/.claude.json (user scope) on activation.
  # Secrets are referenced as ${ENV_VAR} so no plaintext lands in the nix store;
  # Claude Code expands them at runtime.
  mcpServers = {
    jetbrains = {
      type = "sse";
      url = "http://localhost:64342/sse";
    };
    tmux = {
      type = "stdio";
      command = "npx";
      args = [ "-y" "tmux-mcp" "--shell-type=zsh" ];
      env = { };
    };
    github = {
      type = "http";
      url = "https://api.githubcopilot.com/mcp/";
      headers.Authorization = "Bearer \${GITHUB_MCP_PAT}";
    };
    atuin = {
      type = "stdio";
      command = "node";
      args = [ "/Users/gunnar.bastkowski/git/gbastkowski/mcp-atuin/dist/index.js" ];
      env = { };
    };
    emacs = {
      type = "http";
      url = "http://localhost:8765/mcp";
    };
  };

  mcpJson = pkgs.writeText "claude-mcp.json" (builtins.toJSON mcpServers);
in
{
  home.file.".claude/CLAUDE.md".source = ./claude/CLAUDE.md;
  home.file.".claude/settings.json".source = ./claude/settings.json;
  home.file.".claude/statusline.sh".source = ./claude/statusline.sh;
  home.file.".claude/commands".source = ./claude/commands;
  home.file.".claude/agents".source = ./claude/agents;

  home.activation.claudeMcpServers =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      claude_bin="$(PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:$HOME/.local/bin:$PATH" command -v claude || true)"
      if [ -z "$claude_bin" ]; then
        echo "claude not on PATH, skipping MCP seeding" >&2
      else
        ${pkgs.jq}/bin/jq -c 'to_entries[]' ${mcpJson} | while read -r entry; do
          name="$(${pkgs.jq}/bin/jq -r '.key' <<<"$entry")"
          def="$(${pkgs.jq}/bin/jq -c '.value' <<<"$entry")"
          run "$claude_bin" mcp remove --scope user "$name" >/dev/null 2>&1 || true
          run "$claude_bin" mcp add-json --scope user "$name" "$def"
        done
      fi
    '';
}
