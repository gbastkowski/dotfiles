#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
dotfiles_dir="$(cd "$script_dir/.." && pwd)"
dry_run=0
DEFAULT_BRANCH=${DEFAULT_BRANCH:-main}

usage() {
  cat <<'EOF'
Usage: git-ensure-remotes.sh [--dry-run|-n]

Ensures submodule git remotes are consistent across machines:
- `origin` matches `.gitmodules` URL
- known submodules get an `upstream` remote
- detached HEAD submodules get checked out to their branch
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    -n|--dry-run) dry_run=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) printf 'error: unknown argument: %s\n' "$1" >&2; usage; exit 2 ;;
  esac
done

log() {
  printf '%s\n' "$*"
}

warn() {
  printf 'warning: %s\n' "$*" >&2
}

run() {
  if [ "$dry_run" -eq 1 ]; then
    printf '+ %q' "$1"
    shift
    while [ $# -gt 0 ]; do
      printf ' %q' "$1"
      shift
    done
    printf '\n'
    return 0
  fi

  "$@"
}

ensure_remote() {
  local repo_dir="$1"
  local remote_name="$2"
  local remote_url="$3"

  if [ -z "$remote_url" ]; then
    return 0
  fi

  if git -C "$repo_dir" remote get-url "$remote_name" >/dev/null 2>&1; then
    local current_url
    current_url="$(git -C "$repo_dir" remote get-url "$remote_name")"
    if [ "$current_url" != "$remote_url" ]; then
      log "setting $repo_dir remote $remote_name -> $remote_url"
      run git -C "$repo_dir" remote set-url "$remote_name" "$remote_url"
    fi
  else
    log "adding $repo_dir remote $remote_name -> $remote_url"
    run git -C "$repo_dir" remote add "$remote_name" "$remote_url"
  fi

  if [ "$dry_run" -eq 1 ]; then
    run git -C "$repo_dir" remote set-url --push "$remote_name" "$remote_url"
  else
    git -C "$repo_dir" remote set-url --push "$remote_name" "$remote_url" >/dev/null 2>&1 || true
  fi
}

upstream_url_for_submodule() {
  local submodule_name="$1"

  case "$submodule_name" in
    oh-my-zsh) echo "git@github.com:ohmyzsh/ohmyzsh.git" ;;
    reveal.js) echo "https://github.com/hakimel/reveal.js.git" ;;
    emacs/.emacs.doom) echo "git@github.com:doomemacs/doomemacs.git" ;;
    emacs/.emacs.d) echo "git@github.com:plexus/chemacs2.git" ;;
    *) echo "" ;;
  esac
}

guess_default_branch() {
  local repo_dir="$1"

  local origin_head
  origin_head="$(git -C "$repo_dir" symbolic-ref -q --short refs/remotes/origin/HEAD || true)"
  if [ -n "$origin_head" ] && [[ "$origin_head" == origin/* ]]; then
    echo "${origin_head#origin/}"
  else
    local candidate last_candidate=""
    for candidate in "$DEFAULT_BRANCH" main master; do
      [ -z "$candidate" ] && continue
      if [ "$candidate" = "$last_candidate" ]; then
        continue
      fi
      last_candidate="$candidate"
      if git -C "$repo_dir" show-ref --verify --quiet "refs/remotes/origin/$candidate"; then
        echo "$candidate"
        return
      fi
    done

    echo "$DEFAULT_BRANCH"
  fi
}

ensure_on_branch_if_detached() {
  local repo_dir="$1"
  local desired_branch="$2"

  if [ -z "$desired_branch" ]; then
    desired_branch="$(guess_default_branch "$repo_dir")"
  fi

  if git -C "$repo_dir" symbolic-ref -q HEAD >/dev/null 2>&1; then
    return 0
  fi

  log "$repo_dir is detached; checking out $desired_branch"

  if git -C "$repo_dir" show-ref --verify --quiet "refs/remotes/origin/$desired_branch"; then
    if [ "$dry_run" -eq 1 ]; then
      run git -C "$repo_dir" checkout -B "$desired_branch"
      run git -C "$repo_dir" branch --set-upstream-to="origin/$desired_branch" "$desired_branch"
    else
      git -C "$repo_dir" checkout -B "$desired_branch" >/dev/null
      git -C "$repo_dir" branch --set-upstream-to="origin/$desired_branch" "$desired_branch" >/dev/null 2>&1 || true
    fi
  elif git -C "$repo_dir" show-ref --verify --quiet "refs/heads/$desired_branch"; then
    if [ "$dry_run" -eq 1 ]; then
      run git -C "$repo_dir" checkout "$desired_branch"
    else
      git -C "$repo_dir" checkout "$desired_branch" >/dev/null
    fi
  else
    warn "$repo_dir has no origin/$desired_branch; leaving detached"
  fi
}

cd "$dotfiles_dir"

if [ ! -f .gitmodules ]; then
  warn "no .gitmodules found in $dotfiles_dir; nothing to do"
  exit 0
fi

git config -f .gitmodules --get-regexp '^submodule\..*\.path$' >/dev/null 2>&1 || exit 0

git config -f .gitmodules --get-regexp '^submodule\..*\.path$' \
  | while IFS=' ' read -r key submodule_path; do
    submodule_name="${key#submodule.}"
    submodule_name="${submodule_name%.path}"

    if [ ! -d "$submodule_path/.git" ] && [ ! -f "$submodule_path/.git" ]; then
      warn "submodule $submodule_name ($submodule_path) not initialized; skipping"
      continue
    fi

    origin_url="$(git config -f .gitmodules --get "submodule.$submodule_name.url" || true)"
    ensure_remote "$submodule_path" origin "$origin_url"

    upstream_url="$(upstream_url_for_submodule "$submodule_name")"
    ensure_remote "$submodule_path" upstream "$upstream_url"

    desired_branch="$(git config -f .gitmodules --get "submodule.$submodule_name.branch" || true)"
    ensure_on_branch_if_detached "$submodule_path" "$desired_branch"
  done
