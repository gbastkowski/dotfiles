#!/usr/bin/env bash
set -euo pipefail

XMLLS_VERSION="${XMLLS_VERSION:-latest}"
DEST="${XMLLS_DEST:-$HOME/bin/xmlls}"
RELEASE_BASE="https://github.com/redhat-developer/vscode-xml/releases/download"

usage() {
  cat <<'USAGE'
Usage: install-xmlls.sh [--help]

Downloads the Red Hat LemMinX XML language server binary (xmlls) and
installs it into ~/bin by default.

Environment variables:
  XMLLS_VERSION   Release tag to download (default: latest)
  XMLLS_DEST      Override install path (default: ~/bin/xmlls)
USAGE
}

log() { printf '==> %s\n' "$*"; }

die() { printf 'Error: %s\n' "$*" >&2; exit 1; }

need_cmd() { command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "Unknown option: $1"
      ;;
  esac
  shift
done

need_cmd curl
need_cmd unzip

sha_tool=""
sha_args=()
if command -v shasum >/dev/null 2>&1; then
  sha_tool="shasum"
  sha_args=(-a 256)
elif command -v sha256sum >/dev/null 2>&1; then
  sha_tool="sha256sum"
fi

uname_s=$(uname -s)
uname_m=$(uname -m)

case "$uname_s" in
  Darwin)
    case "$uname_m" in
      arm64|aarch64) base_name="lemminx-osx-aarch_64" ;;
      x86_64) base_name="lemminx-osx-x86_64" ;;
      *) die "Unsupported macOS architecture: $uname_m" ;;
    esac
    ;;
  Linux)
    case "$uname_m" in
      x86_64) base_name="lemminx-linux" ;;
      *) die "Unsupported Linux architecture: $uname_m (no vendor binary available)" ;;
    esac
    ;;
  *)
    die "Unsupported operating system: $uname_s"
    ;;
esac

zip_name="${base_name}.zip"
sha_name="${base_name}.sha256"
release_path="$RELEASE_BASE/$XMLLS_VERSION"

url_zip="$release_path/$zip_name"
url_sha="$release_path/$sha_name"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
archive="$tmp/$zip_name"
sha_file="$tmp/$sha_name"

log "Downloading LemMinX ${XMLLS_VERSION} archive (${zip_name})..."
curl -L --fail --show-error "$url_zip" -o "$archive"

log "Downloading checksum..."
curl -L --fail --show-error "$url_sha" -o "$sha_file"

log "Unpacking archive..."
unzip -qo "$archive" -d "$tmp"

binary_path="$tmp/$base_name"
[[ -f "$binary_path" ]] || die "Failed to unpack LemMinX binary"

if [[ -n "$sha_tool" ]]; then
  expected_sum=$(awk '{print $1}' "$sha_file")
  log "Verifying checksum..."
  actual_sum=$("$sha_tool" "${sha_args[@]}" "$binary_path" | awk '{print $1}')
  [[ "$expected_sum" == "$actual_sum" ]] || die "Checksum mismatch: expected $expected_sum, got $actual_sum"
else
  log "Skipping checksum verification (no sha256 tool found)"
fi

mkdir -p "$(dirname "$DEST")"
install -m 0755 "$binary_path" "$DEST"

log "xmlls installed to $DEST"
