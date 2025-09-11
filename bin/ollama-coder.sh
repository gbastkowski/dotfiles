#!/usr/bin/env bash

# Quick wrapper to chat with a local coding LLM via Ollama.
# Defaults to a good lightweight coder model; will pull if missing.
#
# Usage:
#   ollama-coder.sh [options] [prompt]
#
# Options:
#   -m, --model NAME   Model to use (default: qwen2.5-coder:7b)
#       --repl         Start interactive REPL (same as no prompt)
#       --no-pull      Do not auto-pull missing model
#       --host HOST    OLLAMA_HOST (default inherits env or 127.0.0.1:11434)
#   -h, --help         Show help

set -euo pipefail

model="qwen2.5-coder:7b"
auto_pull=true
repl=false
ollama_host_default="127.0.0.1:11434"
ollama_host="${OLLAMA_HOST:-$ollama_host_default}"

usage() {
  cat <<EOF
Usage: $(basename "$0") [options] [prompt]

Options:
  -m, --model NAME   Model to use (default: $model)
      --repl         Start interactive session
      --no-pull      Do not pull model automatically
      --host HOST    Set OLLAMA_HOST (default: $ollama_host)
  -h, --help         Show this help

Examples:
  $(basename "$0") "Write a Bash script that pings a host."
  $(basename "$0") --repl
  $(basename "$0") -m qwen2.5-coder:14b --repl
EOF
}

have_cmd() { command -v "$1" >/dev/null 2>&1; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    -m|--model) model="$2"; shift 2 ;;
    --no-pull) auto_pull=false; shift ;;
    --repl) repl=true; shift ;;
    --host) ollama_host="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) break ;;
  esac
done

prompt="${1:-}"

if ! have_cmd ollama; then
  echo "Error: 'ollama' not found. Run: bin/ollama-install.sh --yes --start" >&2
  exit 1
fi

export OLLAMA_HOST="$ollama_host"

if $auto_pull; then
  # Pull if model not present
  if ! ollama list | awk '{print $1}' | grep -q "^${model}$"; then
    echo "Pulling model: $model" >&2
    ollama pull "$model"
  fi
fi

if [[ -z "$prompt" ]] || $repl; then
  exec ollama run "$model"
else
  exec ollama run "$model" "$prompt"
fi

