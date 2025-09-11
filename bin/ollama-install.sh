#!/usr/bin/env bash

# Install and optionally start Ollama (macOS/Linux).
# - Idempotent where possible; prints exact actions.
# - Flags:
#   -n, --dry-run   Show commands without executing
#   -y, --yes       Non-interactive install where supported
#   --start         Start the Ollama server after install
#   --host HOST     Set OLLAMA_HOST (default: 127.0.0.1:11434)

set -euo pipefail

dry_run=false
assume_yes=false
do_start=false
ollama_host="127.0.0.1:11434"

log() { printf '%s\n' "$*"; }
run() { if $dry_run; then printf '[dry-run] %s\n' "$*"; else eval "$@"; fi }

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Options:
  -n, --dry-run      Print commands instead of running them
  -y, --yes          Proceed non-interactively when possible
      --start        Start ollama server after install
      --host HOST    Set OLLAMA_HOST (default: $ollama_host)
  -h, --help         Show this help

Examples:
  $(basename "$0") --yes --start
  $(basename "$0") -n
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--dry-run) dry_run=true; shift ;;
    -y|--yes) assume_yes=true; shift ;;
    --start) do_start=true; shift ;;
    --host) ollama_host=${2:-$ollama_host}; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) log "Unknown option: $1"; usage; exit 1 ;;
  esac
done

have_cmd() { command -v "$1" >/dev/null 2>&1; }

os="$(uname -s)"

install_ollama_macos() {
  if have_cmd ollama; then
    log "Ollama already installed (macOS)."
    return 0
  fi
  if have_cmd brew; then
    log "Installing Ollama via Homebrew..."
    run "brew install ollama"
  else
    log "Homebrew not found. Install Homebrew or use the Ollama app from https://ollama.com/download"
    return 1
  fi
}

install_ollama_linux() {
  if have_cmd ollama; then
    log "Ollama already installed (Linux)."
    return 0
  fi
  if $assume_yes; then
    log "Installing Ollama via official install script (non-interactive)."
    run "curl -fsSL https://ollama.com/install.sh | sh"
  else
    log "About to run: curl -fsSL https://ollama.com/install.sh | sh"
    read -r -p "Proceed? [y/N] " ans || true
    if [[ \
      "${ans:-}" =~ ^[Yy]$ \
    ]]; then
      run "curl -fsSL https://ollama.com/install.sh | sh"
    else
      log "Cancelled. Visit https://ollama.com/download for instructions."
      return 1
    fi
  fi
}

start_server() {
  # Start the server in the foreground unless running in dry-run.
  # Users typically keep this in a tmux pane or a systemd user service.
  export OLLAMA_HOST="$ollama_host"
  log "Starting ollama server on $OLLAMA_HOST ..."
  run "nohup ollama serve >/dev/null 2>&1 &"
  log "Server start attempted. Check with: curl http://$OLLAMA_HOST/api/tags || true"
}

case "$os" in
  Darwin) install_ollama_macos ;;
  Linux)  install_ollama_linux ;;
  *)      log "Unsupported OS: $os"; exit 1 ;;
esac

if $do_start; then
  start_server
else
  log "To start the server: OLLAMA_HOST=$ollama_host ollama serve"
fi

log "Tip: pull a coding model next, e.g.: ollama pull qwen2.5-coder:7b"

