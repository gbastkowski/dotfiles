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
  };

  mcpJson = pkgs.writeText "claude-mcp.json" (builtins.toJSON mcpServers);

  # Plugins to install, as plugin@marketplace ids. The marketplaces themselves and
  # the enabled state are declared statically in claude/settings.json
  # (extraKnownMarketplaces / enabledPlugins); this list only drives fetching the
  # plugin content into the writable ~/.claude/plugins cache on activation.
  plugins = [
    "caveman@caveman"
    "ista-presentation@ista-marketplace"
    "gitlab-assistant@mdb"
    "jira-assistant@mdb"
    "mcp-emacs@mcp-emacs"
  ];
in
{
  home.file.".claude/CLAUDE.md".source = ./claude/CLAUDE.md;
  home.file.".claude/statusline.sh".source = ./claude/statusline.sh;
  home.file.".claude/commands".source = ./claude/commands;
  home.file.".claude/agents".source = ./claude/agents;

  # settings.json is seeded as a *mutable* file (not a read-only nix-store symlink)
  # because `claude plugin install` needs to write enabledPlugins into it. The dotfiles
  # copy is the source of truth and is re-copied on every activation, so manual edits
  # made by the CLI to enabledPlugins are expected to be reconciled here.
  home.activation.claudeSettings =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run install -m 0644 ${./claude/settings.json} "$HOME/.claude/settings.json"
    '';

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

  home.activation.claudePlugins =
    lib.hm.dag.entryAfter [ "writeBoundary" "claudeSettings" ] ''
      claude_bin="$(PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:$HOME/.local/bin:$PATH" command -v claude || true)"
      if [ -z "$claude_bin" ]; then
        echo "claude not on PATH, skipping plugin seeding" >&2
      else
        # Marketplaces and enabled state are declared in claude/settings.json, seeded
        # above as a mutable file so `plugin install` can fetch each plugin's content
        # into the ~/.claude/plugins cache (and update enabledPlugins) without EACCES.
        for plugin in ${lib.escapeShellArgs plugins}; do
          run "$claude_bin" plugin install --scope user "$plugin" >/dev/null 2>&1 \
            || echo "failed to install plugin $plugin" >&2
        done
      fi
    '';
}
