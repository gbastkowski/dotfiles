#!/usr/bin/env node

import { spawn, execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import path from "node:path";
import os from "node:os";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import * as z from "zod";

// The server lives in mcp/dist (or mcp/src under tsx); the dotfiles checkout
// is two levels up either way.
const HERE = path.dirname(fileURLToPath(import.meta.url));
const DOTFILES_DIR = path.resolve(HERE, "../..");
const BIN_DIR = path.join(DOTFILES_DIR, "bin");

// home-manager switch needs the user's nix profile on PATH. apply.sh sets it
// itself; passing it here too keeps system-upgrade.sh (brew/yay/doom) working
// when the server runs with a stripped environment.
const NIX_PATH = [
  path.join(os.homedir(), ".nix-profile/bin"),
  "/nix/var/nix/profiles/default/bin",
].join(path.delimiter);

type RunResult = { stdout: string; stderr: string; code: number };

function runScript(script: string, args: string[]): Promise<RunResult> {
  return new Promise((resolve, reject) => {
    const child = spawn("bash", [path.join(BIN_DIR, script), ...args], {
      cwd: DOTFILES_DIR,
      env: {
        ...process.env,
        PATH: `${NIX_PATH}${path.delimiter}${process.env.PATH ?? ""}`,
      },
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk: Buffer) => { stdout += chunk.toString(); });
    child.stderr.on("data", (chunk: Buffer) => { stderr += chunk.toString(); });
    child.on("error", (error: Error) => {
      reject(new Error(`failed to start ${script}: ${error.message}`));
    });
    child.on("close", (code) => {
      resolve({ stdout, stderr, code: code ?? -1 });
    });
  });
}

function detectTarget(): string {
  const host = os.hostname();
  if (host.startsWith("deess1mac")) return "ista-dotfiles";
  if (host.startsWith("akiko")) return "akiko-dotfiles";
  return `unknown (host: ${host})`;
}

function git(args: string[]): Promise<string> {
  return new Promise((resolve) => {
    execFile("git", args, { cwd: DOTFILES_DIR }, (error, stdout) => {
      resolve(error ? `(git error: ${error.message})` : stdout.trim());
    });
  });
}

const server = new McpServer(
  { name: "mcp-dotfiles", version: "0.1.0" },
  {
    capabilities: { tools: {} },
    instructions:
      "Manage Gunnar's dotfiles: apply the home-manager configuration and run full system upgrades. Check the status tool before destructive actions.",
  },
);

const ScriptArgsSchema = z.object({
  args: z
    .array(z.string())
    .optional()
    .describe("Extra arguments passed through to the script"),
});

server.registerTool(
  "apply",
  {
    title: "Apply dotfiles",
    description:
      "Run bin/apply.sh: home-manager switch -b backup --flake <dotfiles>#<host-target>. Makes the current dotfiles configuration live. Host is detected from the hostname (deess1mac* -> ista-dotfiles, akiko* -> akiko-dotfiles). Long-running.",
    inputSchema: ScriptArgsSchema,
  },
  async (input) => {
    try {
      const { stdout, stderr, code } = await runScript("apply.sh", input.args ?? []);
      const text = [stdout, stderr].filter(Boolean).join("\n") || "(no output)";
      return {
        content: [{ type: "text", text: `apply.sh exited with ${code}\n${text}` }],
        structuredContent: { exitCode: code, stdout, stderr },
        isError: code !== 0,
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { isError: true, content: [{ type: "text", text: `apply failed: ${message}` }] };
    }
  },
);

server.registerTool(
  "system_upgrade",
  {
    title: "System upgrade",
    description:
      "Run bin/system-upgrade.sh: OS package updates (softwareupdate/brew or yay/hyprpm depending on host), npm global updates, git pull of the dotfiles (re-execs itself if updated), home-manager switch, doom emacs upgrade. Can take many minutes; do not interrupt.",
    inputSchema: ScriptArgsSchema,
  },
  async (input) => {
    try {
      const { stdout, stderr, code } = await runScript("system-upgrade.sh", input.args ?? []);
      const text = [stdout, stderr].filter(Boolean).join("\n") || "(no output)";
      return {
        content: [{ type: "text", text: `system-upgrade.sh exited with ${code}\n${text}` }],
        structuredContent: { exitCode: code, stdout, stderr },
        isError: code !== 0,
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { isError: true, content: [{ type: "text", text: `system-upgrade failed: ${message}` }] };
    }
  },
);

server.registerTool(
  "status",
  {
    title: "Dotfiles status",
    description:
      "Read-only overview: hostname, detected flake target, dotfiles dir, git branch/HEAD and dirty state.",
    inputSchema: z.object({}),
  },
  async () => {
    const head = await git(["rev-parse", "--short", "HEAD"]);
    const branch = await git(["branch", "--show-current"]);
    const dirty = await git(["status", "--porcelain"]);
    const target = detectTarget();
    const text = [
      `Dotfiles: ${DOTFILES_DIR}`,
      `Host: ${os.hostname()}`,
      `Flake target: ${target}`,
      `Branch: ${branch || "(detached)"}`,
      `HEAD: ${head}`,
      dirty ? `Uncommitted changes:\n${dirty}` : "Working tree clean",
    ].join("\n");
    return { content: [{ type: "text", text }] };
  },
);

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("mcp-dotfiles server ready (stdio)");
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
