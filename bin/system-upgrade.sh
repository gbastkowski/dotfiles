#!/usr/bin/env bash

SCRIPTPATH=$(dirname $0)
DOTFILES_DIR="$HOME/git/gbastkowski/dotfiles"
ORIGINAL_DIR=$(pwd)

# Change to dotfiles directory
cd "$DOTFILES_DIR" || { echo "Error: Cannot find dotfiles directory at $DOTFILES_DIR"; exit 1; }

OSTYPE=$($SCRIPTPATH/ostype.sh)
# [ -f $HOME/.bashrc ] && $HOME/.bashrc

case "$OSTYPE" in
  arch*)    . $SCRIPTPATH/linux.include.sh ;;
  darwin*)  . $SCRIPTPATH/macos.include.sh ;;
  termux*)  . $SCRIPTPATH/termux.include.sh ;;
  *)        echo "unknown: $OSTYPE" ; exit 1 ;;
esac

upgrade_system_and_packages

upgrade_python_packages

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]
then
  echo "updating sdkman"
  source "$HOME/.sdkman/bin/sdkman-init.sh"
  sdk selfupdate
  sdk upgrade
fi

echo "updating dotfiles ..."
echo

echo "updating oh-my-zsh ..."
git -C zsh/.oh-my-zsh fetch --all
EDITOR=vim git -C zsh/.oh-my-zsh merge upstream/master
echo

echo "updating powerlevel10k ..."
git -C powerlevel10k pull
echo

echo "updating zsh-vi-mode ..."
git -C zsh-vi-mode pull
echo

echo "updating chemacs2 ..."
git -C emacs/.emacs.d fetch --all
EDITOR=vim git -C emacs/.emacs.d checkout main
EDITOR=vim git -C emacs/.emacs.d merge upstream/main
EDITOR=vim git -C emacs/.emacs.d push
echo

# echo "updating spacemacs ..."
# git -C emacs/.emacs.spacemacs fetch --all
# EDITOR=vim git -C emacs/.emacs.spacemacs checkout develop
# EDITOR=vim git -C emacs/.emacs.spacemacs merge upstream/develop
# EDITOR=vim git -C emacs/.emacs.spacemacs push
# EDITOR=vim git -C emacs/.emacs.spacemacs checkout gunnar
# EDITOR=vim git -C emacs/.emacs.spacemacs merge develop
# EDITOR=vim git -C emacs/.emacs.spacemacs push
# echo

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
git -C zsh/.oh-my-zsh push origin
git -C emacs/.emacs.d push origin
echo

echo "pushing dotfiles to origin ..."
git add .
EDITOR=vim git commit -m "Update dotfiles and submodules"
git push origin

echo "current state:"
git status

# Return to original directory
cd "$ORIGINAL_DIR"

echo
echo "done :-)"
