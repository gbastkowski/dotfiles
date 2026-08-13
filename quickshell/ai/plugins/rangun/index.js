// opencode plugin: "ask-rangun" tool — delegates a question to Rangun,
// the Hermes/Open WebUI assistant on smarty:3000 (OpenAI-compatible API).
//
// Key resolution (first match wins):
//   1. env RANGUN_API_KEY
//   2. `pass show <path>`  (default path hermes/api-key, override RANGUN_PASS_PATH)
// Endpoint/model: RANGUN_ENDPOINT (default http://smarty:3000), RANGUN_MODEL
// (default: first model from /api/models).

import { tool } from "@opencode-ai/plugin";
import { execFileSync } from "node:child_process";

function getKey() {
  if (process.env.RANGUN_API_KEY) return process.env.RANGUN_API_KEY.trim();
  const passPath = process.env.RANGUN_PASS_PATH || "private/rangun-openwebui-api-key";
  try {
    const out = execFileSync("pass", ["show", passPath], {
      encoding: "utf8",
      timeout: 10000,
      stdio: ["ignore", "pipe", "ignore"],
    });
    const key = out.split("\n")[0].trim();
    if (key) return key;
  } catch {
    // fall through to the error below
  }
  throw new Error(
    `Rangun API key unavailable: set RANGUN_API_KEY or pass show ${passPath}`
  );
}

async function askRangun(question) {
  const key = getKey();
  const endpoint = process.env.RANGUN_ENDPOINT || "http://smarty:3000";
  const auth = { Authorization: "Bearer " + key };

  let modelId = process.env.RANGUN_MODEL || "";
  if (!modelId) {
    const res = await fetch(endpoint + "/api/models", { headers: auth });
    if (!res.ok) throw new Error("Rangun /api/models HTTP " + res.status);
    const data = await res.json();
    modelId = data.data?.[0]?.id || "";
    if (!modelId) throw new Error("Rangun reported no models");
  }

  const res = await fetch(endpoint + "/api/chat/completions", {
    method: "POST",
    headers: { "Content-Type": "application/json", ...auth },
    body: JSON.stringify({
      model: modelId,
      messages: [{ role: "user", content: question }],
      stream: false,
    }),
  });
  if (!res.ok) {
    const body = await res.text();
    throw new Error(`Rangun HTTP ${res.status}: ${body.slice(0, 200)}`);
  }
  const data = await res.json();
  const content = data.choices?.[0]?.message?.content;
  return typeof content === "string" && content.length > 0 ? content : "(Rangun returned no text)";
}

export const plugin = async () => ({
  tool: {
    "ask-rangun": tool({
      description:
        "Ask the Rangun assistant (Hermes/Open WebUI on smarty:3000). Use this to delegate a question to Rangun and return its answer.",
      args: {
        question: tool.schema.string().describe("The question to ask Rangun"),
      },
      execute: async ({ question }) => askRangun(question),
    }),
  },
});
