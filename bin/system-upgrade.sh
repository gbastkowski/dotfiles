#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(cd "$SCRIPTPATH/.." && pwd)"
ORIGINAL_DIR="$(pwd)"

sdkman_noninteractive_upgrade() {
  if ! declare -F sdk >/dev/null 2>&1; then
    echo "warning: sdk command not available; skipping sdkman upgrade"
    return 0
  fi

  if ! declare -F __sdkman_secure_curl >/dev/null 2>&1; then
    echo "warning: sdkman helpers missing; skipping sdkman upgrade"
    return 0
  fi

  if [ -z "${SDKMAN_CANDIDATES_DIR:-}" ] || [ ! -d "$SDKMAN_CANDIDATES_DIR" ]; then
    echo "warning: no sdkman candidates directory; skipping sdkman upgrade"
    return 0
  fi

  local candidate_path candidate default_version
  local -a installed_candidates=()

  for candidate_path in "$SDKMAN_CANDIDATES_DIR"/*; do
    [ -d "$candidate_path" ] || continue
    candidate="$(basename "$candidate_path")"
    installed_candidates+=("$candidate")
  done

  if [ ${#installed_candidates[@]} -eq 0 ]; then
    echo "no sdkman candidates installed, skipping sdkman upgrade"
    return 0
  fi

  local -a upgrade_candidates=()
  local -a upgrade_versions=()
  local sdkman_api="${SDKMAN_CANDIDATES_API:-https://api.sdkman.io/2}"

  for candidate in "${installed_candidates[@]}"; do
    default_version="$(
      __sdkman_secure_curl "${sdkman_api}/candidates/default/${candidate}" 2>/dev/null || true
    )"

    if [ -z "$default_version" ]; then
      continue
    fi

    if [ -d "${SDKMAN_CANDIDATES_DIR}/${candidate}/${default_version}" ]; then
      continue
    fi

    if sdk list "$candidate" 2>/dev/null | LC_ALL=C grep -Fq "$default_version"; then
      upgrade_candidates+=("$candidate")
      upgrade_versions+=("$default_version")
    else
      echo "warning: sdkman candidate ${candidate} default ${default_version} unavailable on ${SDKMAN_PLATFORM:-unknown}; skipping"
    fi
  done

  if [ ${#upgrade_candidates[@]} -eq 0 ]; then
    echo "All sdkman candidates already up to date or skipped"
    return 0
  fi

  local previous_auto_answer="${sdkman_auto_answer:-false}"
  sdkman_auto_answer=true
  local idx candidate_name candidate_version
  for idx in "${!upgrade_candidates[@]}"; do
    candidate_name="${upgrade_candidates[$idx]}"
    candidate_version="${upgrade_versions[$idx]}"
    echo "Installing ${candidate_name} default ${candidate_version} ..."
    __sdk_install "$candidate_name" "$candidate_version" || echo "warning: failed to install ${candidate_name} ${candidate_version}"
  done
  sdkman_auto_answer="$previous_auto_answer"
}

# Change to dotfiles directory
cd "$DOTFILES_DIR" || { echo "Error: Cannot find dotfiles directory at $DOTFILES_DIR"; exit 1; }

OSTYPE="$("$SCRIPTPATH/ostype.sh")"
# [ -f $HOME/.bashrc ] && $HOME/.bashrc

case "$OSTYPE" in
  arch*)    . "$SCRIPTPATH/linux.include.sh" ;;
  darwin*)  . "$SCRIPTPATH/macos.include.sh" ;;
  termux*)  . "$SCRIPTPATH/termux.include.sh" ;;
  *)        echo "unknown: $OSTYPE" ; exit 1 ;;
esac

upgrade_system_and_packages

upgrade_python_packages

echo "normalizing git remotes (submodules) ..."
"$SCRIPTPATH/git-ensure-remotes.sh" || echo "warning: failed to normalize submodule remotes"
echo

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]
then
  echo "updating sdkman"
  # shellcheck disable=SC1090
  source "$HOME/.sdkman/bin/sdkman-init.sh"
  sdk selfupdate || echo "warning: sdkman selfupdate failed"
  sdkman_noninteractive_upgrade
  echo
fi

if command -v npm >/dev/null 2>&1
then
  echo "updating ccline (npm) ..."
  npm update -g @cometix/ccline || echo "warning: failed to update ccline"
  npm update -g tweakcc         || echo "warning: failed to update tweakcc"
  npm update -g @fission-ai/openspec || echo "warning: failed to update openspec"
  echo
else
  echo "npm not found, skipping ccline update"
  echo
fi

echo "updating dotfiles ..."
echo

echo "updating oh-my-zsh ..."
git -C zsh/.oh-my-zsh checkout -q master 2>/dev/null || true
git -C zsh/.oh-my-zsh fetch --all
EDITOR=vim git -C zsh/.oh-my-zsh merge upstream/master
echo

echo "updating powerlevel10k ..."
git -C powerlevel10k pull
echo

echo "updating zsh-vi-mode ..."
git -C zsh-vi-mode checkout -q master 2>/dev/null || true
git -C zsh-vi-mode pull
echo

echo "updating chemacs2 ..."
git -C emacs/.emacs.d fetch --all
EDITOR=vim git -C emacs/.emacs.d checkout main
if git -C emacs/.emacs.d show-ref --verify --quiet refs/remotes/upstream/main; then
  EDITOR=vim git -C emacs/.emacs.d merge upstream/main
else
  EDITOR=vim git -C emacs/.emacs.d merge upstream/master
fi
EDITOR=vim git -C emacs/.emacs.d push
echo

echo "updating doomemacs ..."
git -C emacs/.emacs.doom fetch --all
EDITOR=vim git -C emacs/.emacs.doom checkout master
EDITOR=vim git -C emacs/.emacs.doom merge upstream/master
EDITOR=vim git -C emacs/.emacs.doom push
echo

echo "updating reveal.js ..."
git -C reveal.js fetch --all
EDITOR=vim git -C reveal.js merge upstream/master
echo

echo "pushing submodules to origin ..."
git -C zsh/.oh-my-zsh pull --rebase origin master || true
git -C zsh/.oh-my-zsh push origin
git -C reveal.js pull --rebase origin master || true
git -C reveal.js push origin master
git -C emacs/.emacs.d push origin
echo

echo "pushing dotfiles to origin ..."
git add .
EDITOR=vim git commit -m "Update dotfiles and submodules"
git pull --rebase origin main
git push origin

echo "current state:"
git status

# Return to original directory
cd "$ORIGINAL_DIR" || exit 1

echo
echo "done :-)"
